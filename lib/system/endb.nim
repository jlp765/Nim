#
#
#            Nim's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This file implements the embedded debugger that can be linked
# with the application. Mostly we do not use dynamic memory here as that
# would interfere with the GC and trigger ON/OFF errors if the
# user program corrupts memory. Unfortunately, for dispaying
# variables we use the ``system.repr()`` proc which uses Nim
# strings and thus allocates memory from the heap. Pity, but
# I do not want to implement ``repr()`` twice.

const
  EndbBeg = "endb| "

  DbgLen = 127                # endb dbg break points

  MaxRep = 50                 # number of repetitive step (s)
  MaxDepth = 4                # default of the md command

type
  BreakpointFilename = object
    b: ptr Breakpoint
    filename: StaticStr

  DbgState = enum
    dbgOff,          # debugger is turned off
    dbgStepInto,     # debugger is in tracing mode
    dbgStepIntoSys,  # debugger traces into system files
    dbgStepOver,     # step over proc calls
    dbgSkipCurrent,  # skip ahead until finished current proc or level
    dbgQuiting,      # debugger wants to quit
    dbgBreakpoints   # debugger is only interested in breakpoints

  VarLocationObj = tuple[procVarName: StaticStr, lineNr: int]

  VarsInitObj = object
    filename: StaticStr
    vars: seq[VarLocationObj]

  HistoryObj = object
    filename*: cstring
    procname*: cstring
    line*: int
    src*: StaticStr

{.deprecated: [TStaticStr: StaticStr, TBreakpointFilename: BreakpointFilename,
              TDbgState: DbgState].}

var
  dbgUser: StaticStr          # buffer for user input; first command is ``step_into``
                              # needs to be global cause we store the last command
                              # in it
  dbgState: DbgState          # state of debugger
  dbgExpand: bool = true      # show values when list local or global variables
  #dbgSkipToFrame: PFrame     # frame to be skipped to
  dbgPrevStackFrame: PFrame   # checked against framePtr for sfHistory
  dbgStr: StaticStr           # command to repeat (if any)
  dbgNr = 0                   # nr of extra step repetitions 0..MaxRep

  maxDisplayRecDepth: int = MaxDepth  # do not display too much data for nested objects

  srcCodeLine: StaticStr

  sfHistory: seq[HistoryObj] = @[]   # history displayed by the "where" command

#-------------------- Support procs ----------------------------------------

proc hash(x: string): uint =
  var h: uint = 0
  for i in 0 .. x.len-1:
    h = h + ord(x[i]).uint
  result = h

proc `contains`(a: StaticStr, b: cstring): bool =
  result = contains(a.data, b)

proc endsWith(a: StaticStr, b: cstring): bool =
  if b.len > a.len: return false
  var
    i = 0
    bLen = b.len - 1
    aLen = a.len - 1
  while i <= bLen:
    if b[bLen - i] != a.data[aLen - i]: return false
    inc i
  return true

proc containsCount(a, b: cstring): int =
  # search all of a to find how many of b it contains
  var
    st = 0
    bLen = b.len - 1
    mtch = 0
  for i in 0 .. a.len-1:
    if (st > 0) and (a[i] == b[mtch]) and (mtch == bLen):
      inc result
      st = 0
      mtch = 0
      continue
    if (st > 0) and (a[i] != b[mtch]):
      st = 0    # keep trying
      mtch = 0
    elif (st > 0) and (a[i] == b[mtch]):
      inc mtch
    elif (st == 0) and (a[i] == b[mtch]):
      inc st    # first match
      inc mtch
    if (st > 0) and (mtch > bLen):
      inc result
      st = 0
      mtch = 0

proc hasExt(s: cstring): bool =
  # returns true if s has a filename extension
  var i = s.len-1
  while i > 0:
    if s[i] == '.': return true
    dec i

proc scanWord(src: cstring, a: var StaticStr, start: int): int  # fwd decl

proc getEndingAfter(a, c: cstring): cstring =
  # return the portion of StaticStr after the c
  # else return a
  var
    res: StaticStr
    i = findFirst(a, c)
  if i >= 0 and a.len > 0 and i+c.len < a.len:
    res.assign(cast[cstring](unsafeAddr(a[i+c.len])))
    return $res.data
  else:
    return a

proc write(f: File, s: StaticStr) =
  write(f, $s.data)

proc writeLine(f: File, s: StaticStr) =
  writeLine(f, $s.data)

proc write(f: File, p: varargs[cstring]) =
  for s in items(p):
    write(f, s)

proc write(f: File, p: varargs[StaticStr]) =
  for s in items(p):
    write(f, $s.data)

proc writeLine(f: File) =
  write(f, "\n")

proc writeLine(f: File, p: varargs[cstring]) {.inline.} =
  for s in items(p):
    write(f, s)
  write(f, "\n")

proc writeBeg(stream: File, p: varargs[string]) =
  stream.write(EndbBeg)
  for s in items(p):
    stream.write($s)

proc writeLineBeg(stream: File, p: varargs[string]) =
  stream.writeBeg(p)
  stream.write("\n")

proc readLine(f: File, line: var StaticStr): bool =
  while true:
    var c = c_fgetc(f)
    if c < 0'i32:
      if line.len > 0: break
      else: return false
    if c == 10'i32: break # LF
    if c == 13'i32:  # CR
      c = c_fgetc(f) # is the next char LF?
      if c != 10'i32: discard c_ungetc(c, f) # no, put the character back
      break
    add line, chr(int(c))
  result = true

proc print(s: StaticStr) =
  stdout.write($s.data)
proc print[T: string|cstring](s: T) =
  stdout.write(s)

proc printLn(s: StaticStr) =
  stdout.writeLine(s.data)
  stdout.flushFile()
proc printLn[T: string|cstring](s: T) =
  stdout.writeLine($s)
  stdout.flushFile()

proc print(i: int) {.inline.} = stdout.write(i)

proc printLn(i: int) {.inline.} = stdout.writeLine(i)

proc printBeg(p: varargs[string]) =
  stdout.write(EndbBeg)
  for s in items(p):
    stdout.write($s)

proc printLnBeg() =
  stdout.writeLine(EndbBeg)
proc printLnBeg(stream: File) =
  stream.writeLine(EndbBeg)

proc printLnEnd() {.inline.} =
  stdout.writeLine("")
proc printLnEnd(stream: File) {.inline.} =
  stream.writeLine("")

proc printLnDone() {.inline.} =
  stdout.flushFile()
  discard
proc printLnDone(stream: File) {.inline.} =
  stream.flushFile()
  discard

proc print(p: varargs[string]) =
  for s in items(p):
    stdout.write($s)

proc printLn(p: varargs[string]) =
  for s in items(p):
    stdout.write($s)
  stdout.write("\n")

proc dbgMsgOut(p: varargs[string]) =
  # the endb| marker is for easy recognition of debugger
  # output for external frontends.
  print(EndbBeg)
  printLn(p)

# ------------------- StaticStr scanning support ---------------------

proc scanAndAppendWord(src: cstring, a: var StaticStr, start: int): int =
  result = start
  # skip whitespace:
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of 'a'..'z', '0'..'9': add(a, src[result])
    of '_': discard    # just skip it
    of 'A'..'Z': add(a, chr(ord(src[result]) - ord('A') + ord('a')))
    else: break
    inc(result)

proc scanWord(src: cstring, a: var StaticStr, start: int): int =
  setlen(a)
  result = scanAndAppendWord(src, a, start)

proc scanWord(src: StaticStr, a: var StaticStr, start: int): int =
  setlen(a)
  result = scanAndAppendWord(src.data, a, start)

proc scanAndAppendEval(src: cstring, a: var StaticStr, start: int): int =
  result = start
  # skip whitespace:
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of 'a'..'z', '0'..'9', '_': add(a, src[result])
    of 'A'..'Z': add(a, chr(ord(src[result]) - ord('A') + ord('a')))
    else: break
    inc(result)

proc scanAndAppendNonWord(src: cstring, a: var StaticStr, start: int): int =
  var inQuote = false
  result = start
  # skip whitespace:
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of '\0': break
    of 'a'..'z', '0'..'9', '_', 'A'..'Z', ' ', '\t':
      if not inQuote: break
      else: add(a, src[result])
    of '\\':
      if inQuote:
        add(a, src[result])   # esc char
        inc(result)
        add(a, src[result])   # escaped char
      else: add(a, src[result])
    of '"':
      inQuote = not inQuote
      add(a, src[result])
    else:
      add(a, src[result])
    inc(result)
    if not inQuote: break

proc scanNonWord(src: cstring, a: var StaticStr, start: int): int =
  setlen(a)
  result = scanAndAppendNonWord(src, a, start)

proc scanFilename(src: cstring, a: var StaticStr, start: int): int =
  result = start
  setLen a
  while src[result] in {'\t', ' '}: inc(result)
  while src[result] notin {'\t', ' ', '\0'}:
    add(a, src[result])
    inc(result)

proc scanFilenameFromVar(src: cstring, a: var StaticStr, start: int): int =
  # extract "filename" into `a` from `src` where `src` like "filename.varname"
  result = start
  setLen a
  while src[result] in {'\t', ' '}: inc(result)
  while src[result] notin {'.', ' ', '\0'}:
    add(a, src[result])
    inc(result)

proc scanNumber(src: cstring, a: var int, start: int): int =
  # return the position src has been scanned to, parsed number is in ``a``
  result = start
  a = 0
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of '0'..'9': a = a * 10 + ord(src[result]) - ord('0')
    of '_': discard       # skip underscores (nice for long line numbers)
    else: break
    inc(result)

# ------------------- sourceFile support ------------------------------------

proc isSystemFile(fn: cstring): bool {.inline.} =
  result = false
  for s in sysStr:
    if contains(fn, s): return true

proc fileMatchesSource(fp: PFrame): bool =
  # true if framePtr.filename matches
  #    the current sourceFile.filename
  # true if not a system file
  var i = 0
  result = false
  while true:
    let x = dbgFilenames[i]
    if isNil(x) or x[0] == '\0': break
    if isSystemFile(x):
      inc i
      continue
    if x.contains(fp.filename):
      result = true
      break
    inc i

proc getCurrentFileNameNoExt(fp: PFrame): cstring =
  # drop the .ext from the file name, returned
  # as a separate buffered cstring
  var
    t: StaticStr
    i = fp.filename.len - 1
  t.assign(fp.filename)
  while i > 0:
    if t.data[i] == '.':
      t.data[i] = '\0'
      t.len = i
      break
    dec i
  result = $t.data

proc addCurrentFrame(h: var seq[HistoryObj]) =
  var hObj: HistoryObj
  hObj.filename = framePtr.filename
  hObj.procname = framePtr.procname
  hObj.line = framePtr.line
  hObj.src.assign(srcCodeLine.data)
  h.add(hObj)

proc validFilename(fn: cstring): bool =
  # returns true if filename (no extension) is in dbgFilenames list
  var ss: StaticStr
  ss.assign(fn)
  ss.add(".nim")
  for i in 0 .. <dbgFilenameLen:
    if dbgFilenames[i] == ss.data: return true
  result = false

#----------------------- top level actions --------------------------

proc dbgRepr(p: pointer, typ: PNimType): string       # fwd decl

proc getGenericTypeName(t: PNimType): cstring =
  ## return type string with the leading ty removed
  ## "tyString" -> "string"
  var tname, ss: StaticStr
  tname.assign($t.kind)
  # LCase to UCase
  tname.data[2] = cast[char](cast[int](tname.data[2]) + 32)  # 'a' = chr(ord('A')+32)
  let tStr = $cast[cstring](addr(tname.data[2]))
  tname.assign(tStr)   # ty prefix removed
  case hash($tname.data)
  of hash("var"):
    ss.assign("var ")
    ss.add(getGenericTypeName(t.base))
    return $ss.data
  of hash("sequence"):    # composite types need to return the base type
    ss.assign(getGenericTypename(t.base))
    return $ss.data
  of hash("array"):
    ss.assign(getGenericTypename(t.base))
    return $ss.data
  else: discard
  result = $tname.data

proc dbgVarOut(stream: File, filename: cstring, lineNr: int, varname, typeStr, valueStr: cstring) =
  # value can be nil, "", "<undefined>" or "<some value string>"
  if filename.contains("."):
    writeBeg(stream, $filename, "(", $lineNr, "): \t", $varname, ": ", $typeStr)
  else:
    writeBeg(stream, $filename, ".nim(", $lineNr, "): \t", $varname, ": ", $typeStr)
  if valueStr.len == 0:
    writeLine(stream, "")
  else:
    writeLine(stream, " = ", $valueStr)

proc writeVariable(stream: File, fp: PFrame, slot: VarSlot) =
  # for variable i=654, output  (as in the "p" command)
  # i: int = 654
  # only call this if you know the variable has been initialized
  # else use writeLocalImpl()
  var
    v: StaticStr
  v.assign($getEndingAfter(slot.name, "."))   # just the varname
  dbgVarOut(stream, fp.filename, slot.lineNr, $v.data, $getGenericTypename(slot.typ), $dbgRepr(slot.address, slot.typ))

proc listVariables(stream: File, fp: PFrame) =
  # output:   lineNr: varname: type
  #
  #   Variables (2 slots):
  #   14: tst_endb.s: string
  #   15: tst_endb.i: int
  #
  # only for current filename
  #
  var fn = getCurrentFileNameNoExt(fp)
  var
    isThisFile: array[0..VarSize, bool]
    vCnt = 0
    vname: StaticStr
  for i in 0.. <getVarDataLen():
    let vt = getVarData(i)
    if vt.varName.data.startsWith(fn):
      isThisFile[i] = true
      inc vCnt
    else:
      isThisFile[i] = false
  writeLineBeg(stream, "Variables (" & $vCnt & "):")
  for i in 0.. <getVarDataLen():
    if isThisFile[i]:
      let vt = getVarData(i)
      vname.assign($getEndingAfter(vt.varName.data, "."))
      dbgVarOut(stream, $fn, vt.lineNr, $vname.data, $vt.typeNameStr.data, nil)
  printLnDone(stream)

proc writeLocalImpl(stream: File, fp: PFrame, i: int) =
  # Checks whether a variable has been initialised, and only prints the dbgRepr() if initialized
  var fn, ss: StaticStr
  fn.assign(getCurrentFileNameNoExt(fp))
  let slot = getLocal(fp, i)
  ss.assign(fn)
  ss.add(".")
  ss.add(slot.name)
  let v = getVarData(ss.data)
  if v.address != nil:
    ss.assign($getEndingAfter(v.varName.data, "."))
  else: ss.assign(slot.name)
  if v.lineNr == 0:
    # var unregistered (but frame has supplied minimal details).
    # So, has no type representation
    dbgVarOut(stream, fp.filename, fp.line, $ss.data, $getGenericTypename(slot.typ), "<undefined>")
  elif dbgExpand and (fp.line <= v.lineNr):
    # variable registered, but un-initialized yet, so <undefined>
    dbgVarOut(stream, fp.filename, v.lineNr, $ss.data, $v.typeNameStr.data, "<undefined>")
  elif dbgExpand and (fp.line > v.lineNr):
    # var registered, and initialized
    dbgVarOut(stream, fp.filename, v.lineNr, $ss.data, $v.typeNameStr.data, $dbgRepr(v.address, v.typ))
  else:
    # don't expand the variable value (but variable has been registered)
    dbgVarOut(stream, fp.filename, v.lineNr, $ss.data, $v.typeNameStr.data, nil)

proc writeLocalImpl(stream: File, fp: PFrame, vname: cstring): bool =
  # variable is checked whether it has been initialized before dbgRepr()
  var cs: cstring = $getEndingAfter(vname,".")
  var i: int = -1
  if cs.len == 0: cs = vname
  i = getLocalVarIndex(fp, cs)
  if i > -1:
    writeLocalImpl(stream, fp, i)
    result = true
  result = false

#[
proc listFrame(stream: File, f: PFrame) =
  # output:   (for expanded output)  ("sf" command)
  #    Frame: (2 slots):
  #    s: string = 00000000004E1188"Greetings"
  #    i: int = 654
  writeLineBeg("Stack Frame")
  writeBeg($f.filename, "(", $f.line, "): \t", $f.procname)
  if f == framePtr:
    writeLine(stream, " -> ", srcCodeLine.data)
  printLnDone(stream)
]#

proc listLocals(stream: File, fp: PFrame) =
  # output:   (for expanded output)  ("l" command)
  #    Locals: (2 slots):
  #    s: string = 00000000004E1188"Greetings"
  #    i: int = 654
  writeLineBeg(stream, "Locals:")
  for i in 0 .. fp.len-1:
    writeLocalImpl(stream, fp, i)
  printLnDone(stream)

template writeGlobalImpl(stream: File, fp: PFrame, i: int) =
  # checks and only displays repr of variable if current line
  # is after the global variable has been defined.
  # If dbgExpand is false, only display the var name
  var ss1, ss2: StaticStr
  let slot = getGlobal(i)
  # get filename
  ss1.setLen(0)
  var j = scanFilenameFromVar(slot.name, ss1, 0)
  # get var name
  ss2.setLen(0)
  j = scanFilenameFromVar(slot.name, ss2, j+1)
  if dbgExpand and (fp.line > slot.lineNr and slot.lineNr > 0):
    # Global var registered and initialized
    dbgVarOut(stream, ss1.data, slot.lineNr, ss2.data, $slot.typStr, $dbgRepr(slot.address, slot.typ))
  elif dbgExpand:
    # Global var registered but uninitialized ( = <undefined>)
    dbgVarOut(stream, ss1.data, slot.lineNr, ss2.data, $slot.typStr, "<undefined>")
  else:
    # don't expand
    dbgVarOut(stream, ss1.data, slot.lineNr, ss2.data, $slot.typStr, nil)

proc writeGlobalImpl(stream: File, fp: PFrame, vname: cstring): bool =
  var
    i: int = -1
    ss: StaticStr
  if vname.contains("."):
    ss.assign(vname)
  else:
    ss.assign(getCurrentFileNameNoExt(fp))
    ss.add(".")
    ss.add(vname)
  i = getGlobalVarIndex(ss.data)
  if i > -1:
    writeGlobalImpl(stream, fp, i)
    result = true
  else: result = false

proc listGlobals(stream: File, f: PFrame) =
  # output:   (for expanded output)  ("l" command)
  #    Globals:
  #    myfile.nim(23): i: int = 123
  #    myfile.nim(24): s: string = 00000000004E1158"Hello there"
  writeLineBeg(stream, "Globals:")
  for i in 0 .. getGlobalLen()-1:
    writeGlobalImpl(stream, f, i)
  printLnDone(stream)

proc listBreakPoints() =
  # "bp" command
  printLn("Breakpoints:")
  for b in listBreakpoints():
    print($abs(b.low))
    if b.high != b.low:
      print("..", $abs(b.high))
    print(" ", $b.filename)
    if not b.isActive:
      printLn(" [disabled]")
    else:
      printLn()
  printLnDone()

proc dbgShowExecutionPoint(fp: PFrame) # fwd decl

proc listHistory(stream: File, s: cstring, start: int, fp: PFrame) =
  # if s contains a number (w <n>) then display history as well
  # as the current location
  var
    hLen = 0
    i = scanNumber(s, hLen, start)  # nr hist lines to show
  if hLen == 0:
    dbgShowExecutionPoint(fp)
  else:
    if hLen > sfHistory.len - 1:
      hLen = 0
    else:
      hLen = sfHistory.len - 1 - hLen
    for i in hLen+1 .. <sfHistory.len - 1:  # don't show last entry (yet to run)
      writeLineBeg(stream, $sfHistory[i].filename, "(", $sfHistory[i].line,
          "): \t", $sfHistory[i].procname, " -> ", $sfHistory[i].src.data)
    dbgShowExecutionPoint(fp)  # show current line to be run

proc openAppend(filename: cstring): File =
  var p: pointer = fopen(filename, "ab")
  if p != nil:
    result = cast[File](p)
    writeLine(result, "----------------------------------------")

proc listFilenames() =
  # output:  ("fn" command)
  #    Files:
  #    config\nim.cfg
  #    myfile.nim
  #    lib\system.nim
  printLn(EndbBeg, "Files:")
  var i = 0
  while true:
    let x = dbgFilenames[i]
    if x.isNil: break
    printLn(EndbBeg, $x)
    inc i
  printLnDone()

proc invalidCommand() {.inline.} =
  dbgMsgOut("[Warning] invalid command ignored (type 'h' for help) ")

# ------------------- dbgXXXXXX support ------------------------------------

proc dbgRepr(p: pointer, typ: PNimType): string =
  var cl: ReprClosure
  initReprClosure(cl)
  cl.recDepth = maxDisplayRecDepth
  result = ""
  reprAux(result, p, typ, cl)
  deinitReprClosure(cl)

proc dbgFatal(msg: cstring) =
  dbgMsgOut($msg)
  dbgAborting = true         # the debugger wants to abort
  quit(1)

proc dbgShowExecutionPoint(fp: PFrame) =
  # "w" command, and elswhere
  # don't combine these print statements
  #if stackframe
  printBeg($fp.filename, "(", $fp.line, "): \t", $fp.procname)
  if fp == framePtr:
    print(" -> ")
    print(srcCodeLine)
  printLn()

proc dbgShowCurrentProc(fp: PFrame) =
  # "u" and "d" command
  var sline: StaticStr
  sline.setLen(0)
  if fp == framePtr:    # includes srcCodeLine
    dbgShowExecutionPoint(fp)
  elif fp != nil:       # no srcCodeLine
    for i in 0 .. <sfHistory.len-2:
      if sfHistory[i].line == fp.line and
            sfHistory[i].procname == fp.procname and
            sfHistory[i].filename == fp.filename:
        sline.assign(sfHistory[i].src)
    printLn(EndbBeg, "now at: ", $fp.filename, "(", $fp.line,
            "): \t", $fp.procname, " -> ", $sline.data)

proc dbgHelp() {.inline.} =
  dbgMsgOut("""
list of commands (see the manual for further help):
--------------------- GENERAL -----------------------------
h, help                   display this help message
q, quit                   quit the debugger and the program
<ENTER>                   repeat the previous debugger command
--------------------- EXECUTION ---------------------------
Note: precede the command by a number to repeat: 3 s  for three steps
s, step                   step into a proc, or to next source code line.
ss, sys                   system step into lib/ files (but not lib/system/ files).
                          ss to delve into lib/ files; f, s or n to return to
                          the upper level.
n, next                   next step, stepping over proc calls.
f, skipcurrent            forward steps until the current routine finishes.
c, continue, r, run       continue execution until the next breakpoint (if any).
i, ignore                 continue execution, ignore all breakpoints.
--------------------- BREAKPOINTS ------------------------
b, break [fromline [toline]] [file]
                          set a new breakpoint for line and file.
                          If line or file are omitted the current one is used.
                          [file] does not require the full path or extension.
bp, breakpoints           display the entire breakpoint list.
t, toggle <fromline> [file]
                          enable or disable a breakpoint.
                          [file] does not require the full path or extension.
fn, filenames             list all valid filenames.
--------------------- DATA DISPLAY ------------------------
e, expand                 toggle showing values (for local or global variables).
                          Disable expansion if there are large objects or
                          sequences with lots of data (default is off).
g, globals [file]         display [to file] global variables,
                          which are vars from global scope or an imported lib.
l, locals [file]          display [to file] variables in the current stack frame.
                          (If objects are displayed as [....], set the md higher)
v, variables [file]       display [to file] the variables registered in the
                          current scope, registered while stepping through code.
p, print <expr>           evaluate <expr> and print to screen (if in local or
                          global list).  Evaluates regardless of the Expand setting.
o, out file <expr>        evaluate <expr> and write it to <file>
w, where [n]              display the current execution point, and optionally
                          n lines of preceeding step history.
u, up                     go up in the call stack (doesn't change the execution point).
d, down                   go down in the call stack.
bt, backtrace             display the entire call stack.
md, maxdisplay <integer>  set the display's recursion maximum (0..9).
""")
#sf, stackframe [file]     display current stack frame [and write it to file].

proc getElementAux(p: pointer, n: ptr TNimNode, ename: cstring): tuple[p: pointer, t: PNimType] =
  # not recursive, but get next level only if matches ename
  result = (p: p, t: n.typ)
  case n.kind
  of nkNone: discard
  of nkSlot:
    if n.name.len > 0 and n.name == ename:
      result = (p: cast[pointer](cast[ByteAddress](p) + n.offset), t: n.typ)
      return
  of nkList:
    for i in 0..n.len-1:
      let ct = n.sons[i]
      if ct.name.len > 0 and ct.name == ename:
        result = (p: cast[pointer](cast[ByteAddress](p) + ct.offset), t: ct.typ)
        return
  of nkCase:
    var m = selectBranch(p, n)
    if n.name.len > 0 and n.name == ename:
      result = (p: cast[pointer](cast[ByteAddress](p) + n.offset), t: n.typ)
      return
    result = getElementAux(cast[pointer](cast[ByteAddress](p) + n.offset), n, ename)
    if m != nil: result = getElementAux(p, m, ename)
  else: discard

#-------------------- Parse Eval ----------------------------------------
type
  EvalKind = enum
    ekIndex, ekMember, ekToken, ekFilename
  EvalObj = object
    case kind: EvalKind
    of ekIndex:
      indx: int
    of ekMember:
      member: StaticStr
    of ekToken:
      token: StaticStr
    of ekFilename:
      filename: StaticStr
var
  tt: TNimType
  pt: PNimType = addr(tt)

proc assign(eo: var EvalObj, k: EvalKind, ss: StaticStr) =
  var ival: int = 0
  discard scanNumber($ss.data, ival, 0)
  eo.kind = k
  case eo.kind
    of ekIndex:
      eo.indx = ival
    of ekMember:
      eo.member.assign(ss.data)
    of ekToken:
      eo.token.assign(ss.data)
    of ekFilename:
      eo.filename.assign(ss.data)

proc parseEval(cs: cstring, fp: PFrame): seq[EvalObj] =
  ## return a seq of tokens representing the Eval expression
  ## which allows for any depth of indexes or members, like
  ## filename.varname[2].member[3][1].member[0]
  ##
  ## Only one token is allowed (the varname)
  ## If there is filename.varname, then the varname is the token
  result = @[]
  var
    eo: EvalObj
    ss: StaticStr
    prevKind: EvalKind = ekToken
    v: VarSlot
    i = scanAndAppendEval(cs, ss, 0)
  ss.setLen(0)
  i = scanAndAppendEval(cs, ss, i+1)  # skip print command
  while true:
    if result.len == 0 and validFilename(ss.data):
        eo.assign(ekFilename, ss)
        result.add(eo)
        ss.setLen(0)
    else:
      eo.assign(prevKind, ss)
      result.add(eo)
      ss.setLen(0)
    if i+1 >= cs.len: break
    case cs[i]
      of '.':
        if result.len == 1 and result[0].kind == ekFilename:
          prevKind = ekToken
        else:
          prevKind = ekMember
      of '[': prevKind = ekIndex
      of ' ':
        result = @[]
        dbgMsgOut("[Warning] Misformed variable name")
        return
      else:
        prevKind = ekToken
    ss.setLen(0)
    i = scanAndAppendEval(cs, ss, i+1)
    if i < cs.len and cs[i] == ']': inc i


proc getElementLen(p: pointer, typ: PNimType): int =
  result = 0
  case typ.kind
  of tyCString:  # -> char
    let cs = cast[ptr cstring](p)[]
    result = cs.len
  of tyString:  # -> char
    let s = cast[ptr string](p)[]
    result = s.len
  of tyVar:
    # C++ uses referenced pointers, C uses pointers to pointers ???
    when defined(cpp):
      if (typ.base.kind ==  tyArray):
        result = getElementLen(cast[ptr pointer](p)[], typ.base)
      else:
        result = getElementLen(cast[pointer](p), typ.base)
    else:
      result = getElementLen(cast[ptr pointer](p)[], typ.base)
  of tyArray:
    result = typ.size div typ.base.size
  of tySequence:
    result = cast[int](cast[ptr int](cast[PPointer](p)[])[])
  else: discard

proc getElement(p: pointer, typ: PNimType, ename: cstring): tuple[p: pointer, t: PNimType] =
  # ename is a number for array/sequence (as a string) excluding brackets
  # or the name of the tuple/object element otherwise
  var
    indx = 0
  result = (p, typ)
  case typ.kind
  of tyCString:  # -> char
    discard scanNumber(ename, indx, 0)
    let cs = cast[ptr cstring](p)[]
    if indx < cs.len:
      tt.kind = tyChar
      result = (cast[pointer](unsafeAddr(cs[indx])), pt)
  of tyString:  # -> char
    discard scanNumber(ename, indx, 0)
    let s = cast[ptr string](p)[]
    if indx < s.len:
      tt.kind = tyChar
      result = (cast[pointer](unsafeAddr(s[indx])), pt)
  of tyVar:
    # C++ uses referenced pointers, C uses pointers to pointers ???
    when defined(cpp):
      if (typ.base.kind ==  tyArray):
        result = getElement(cast[ptr pointer](p)[], typ.base, ename)
      else:
        result = getElement(cast[pointer](p), typ.base, ename)
    else:
      result = getElement(cast[ptr pointer](p)[], typ.base, ename)
  of tyArray:
    discard scanNumber(ename, indx, 0)
    let aLen = typ.size div typ.base.size
    if indx < aLen:
      result = (cast[pointer](cast[ByteAddress](p) + indx*typ.base.size), typ.base)
  of tySequence:
    discard scanNumber(ename, indx, 0)
    let sLen = cast[int](cast[ptr int](cast[PPointer](p)[])[])
    if indx < sLen:
      result = (cast[pointer](cast[ByteAddress](cast[PPointer](p)[]) + GenericSeqSize + indx*typ.base.size), typ.base)
  of tyObject, tyTuple:
    var curTyp = typ
    var first = true
    if typ.kind == tyObject:
      curTyp = cast[ptr PNimType](p)[]
    while curTyp != nil and curTyp.node != nil:
      result = getElementAux(p, curTyp.node, ename)
      if first == true and result.p == p and curTyp.base != nil:
        first = false
        curTyp = curTyp.base
      else: break
  else: discard

proc findVarSlot(fp: PFrame, fname, varname: cstring, v: var VarSlot): bool =
  # test local first
  # local has no filename prepended, global has filename
  var
    ss: StaticStr
  if varname.contains("."):
    ss.assign($getEndingAfter(varname, "."))      # get varname from filename.varname
    if getLocalVar(fp, ss.data, v) and v.address != nil: return true
    result = getGlobalVar(varname, v)
  else:
    if getLocalVar(fp, varname, v): return true
    ss.assign(fname)
    ss.add(".")
    ss.add(varname)
    result = getGlobalVar(ss.data, v)

proc dbgEvaluate(stream: File, s: cstring, start: int, fp: PFrame) =
  # "p" command
  var
    tokns = s.parseEval(fp)
    fn, vname: StaticStr
    v: VarSlot
    t: PNimType
    varPtr: tuple[p: pointer, t: PNimType]
  fn.assign(getCurrentFileNameNoExt(fp))  # may be overwritten if diff file var
  for i in 0 .. <tokns.len:
    case (tokns[i]).kind
    of ekFilename:
      vname.assign(tokns[i].filename)
      vname.add(".")
    of ekToken:
      if i == 0:  # no filename prefix
        vname.assign(fn)
        vname.add(".")
        vname.add(tokns[i].token)
      elif i > 1:
        dbgMsgOut("[Warning] Unknown token")
        return
      else:
        vname.add(tokns[i].token)   # already have filename
      if not findVarSlot(fp, fn.data, vname.data, v):
        dbgMsgOut("[Warning] Unknown variable")
        return
      varPtr = (v.address, v.typ)
    of ekMember:
      vname.add(".")
      vname.add(tokns[i].member.data)
      if tokns[i].member == "len":
        var vlen = getElementLen(varPtr.p, varPtr.t)
        dbgVarOut(stream, fp.filename, fp.line, vname.data, $getGenericTypeName(varPtr.t), $vlen)
        return
      else:
        varPtr = getElement(varPtr.p, varPtr.t, tokns[i].member.data)
      if varPtr.t == nil:
        dbgMsgOut("[Warning] Unknown member")
        return
    of ekIndex:
      var ss: StaticStr
      ss.assign($tokns[i].indx)
      vname.add("[")
      vname.add($tokns[i].indx)
      vname.add("]")
      varPtr = getElement(varPtr.p, varPtr.t, ss.data)
      if varPtr.t == nil:
        dbgMsgOut("[Warning] Unknown index")
        return
  if tokns.len > 0:
    if tokns.len == 1 or (tokns.len == 2 and tokns[0].kind == ekFilename):
      # just a plain variable
      if not writeLocalImpl(stream, fp, v.name):
        discard writeGlobalImpl(stream, fp, vname.data)
    else:
      # variable has members/index specified  (nil type, then unrecognized member)
      if varPtr.t != nil:
        dbgVarOut(stream, fp.filename, fp.line, $vname.data, $getGenericTypeName(varPtr.t), dbgRepr(varPtr.p, varPtr.t))

proc dbgOut(s: cstring, start: int, currFrame: PFrame) =
  var dbgTemp: StaticStr
  var i = scanFilename(s, dbgTemp, start)
  if dbgTemp.len == 0:
    invalidCommand()
    return
  var stream: File = nil
  if $dbgTemp.data == "stdout":
    dbgEvaluate(stdout, s, i, currFrame)
  else:
    stream = openAppend(dbgTemp.data)
    if stream == nil:
      dbgMsgOut("[Warning] could not open or create file ")
      return
    dbgEvaluate(stream, s, i, currFrame)
    close(stream)

#[
proc dbgStackFrame(s: cstring, start: int, currFrame: PFrame) =
  var dbgTemp: StaticStr
  var i = scanFilename(s, dbgTemp, start)
  if dbgTemp.len == 0:
    # just write it to stdout:
    dbgShowExecutionPoint(currFrame)
  else:
    var stream = openAppend(dbgTemp.data)
    if stream == nil:
      dbgMsgOut("[Warning] could not open or create file ")
      return
    listFrame(stream, currFrame)
    close(stream)
]#

proc dbgWriteStackTrace(f: PFrame) =
  # "bt" command output
  # print a list of proc calls got the program
  # to the current execution point (framePtr)
  const
    firstCalls = 32
  var
    it = f
    i = 0
    total = 0
    tempFrames: array [0..DbgLen, PFrame]
  # setup long head:
  while it != nil and i <= high(tempFrames)-firstCalls:
    tempFrames[i] = it
    inc(i)
    inc(total)
    it = it.prev
  # go up the stack to count 'total':
  var b = it
  while it != nil:
    inc(total)
    it = it.prev
  var skipped = 0
  if total > len(tempFrames):
    # skip N
    skipped = total-i-firstCalls+1
    for j in 1..skipped:
      if b != nil: b = b.prev
    # create '...' entry:
    tempFrames[i] = nil
    inc(i)
  # setup short tail:
  while b != nil and i <= high(tempFrames):
    tempFrames[i] = b
    inc(i)
    b = b.prev
  for j in countdown(i-1, 0):
    if tempFrames[j] == nil:
      printLn("(", $skipped, " calls omitted) ...")
    else:
      print(tempFrames[j].filename)
      if tempFrames[j].line > 0:
        print("(", $tempFrames[j].line, ")")
      printLn(" ", $tempFrames[j].procname)

proc dbgToggleExpand() =
  # enable/disable evaluating variable when listed (local, global, stack frame)
  dbgExpand = not dbgExpand
  printLn(if dbgExpand: "variable expanding: [enabled]" else: "variable expanding: [disabled]")

proc dbgAddHistory(fp: PFrame) =
  # sfHistory is used to display prev debugging steps
  if sfHistory.len == 0:
    sfHistory.addCurrentFrame()
  else:
    let f = sfHistory[sfHistory.len-1]
    if f.line != fp.line or
        f.procname != fp.procname or
        f.filename != fp.filename:
      sfHistory.addCurrentFrame()

proc getOptionalOutFile(s: string, start: int): File =
  # if no filename supplied in s or an error opening the file, return
  # defaultStream, else the new opened File
  # needs to be closed afterwards
  var
    dbgTemp: StaticStr
    i = scanFilename(s, dbgTemp, start)
  if dbgTemp.len > 0 and $dbgTemp.data != "stdout":
    result = openAppend(dbgTemp.data)
    if result == nil:
      dbgMsgOut("[Warning] could not open or create file ")

#----------------- BreakPoint internals --------------------------------------------
proc parseBreakpoint(fp: PFrame, s: cstring, start: int): Breakpoint =
  var dbgTemp: StaticStr
  var i = scanNumber(s, result.low, start)
  if result.low == 0:
    result.low = fp.line
  i = scanNumber(s, result.high, i)
  if result.high == 0:
    result.high = result.low
  i = scanFilename(s, dbgTemp, i)
  if dbgTemp.len != 0:
    if not hasExt(dbgTemp.data): add(dbgTemp, ".nim")
    result.filename = canonFilename($dbgTemp.data)
    if result.filename.isNil:
      dbgMsgOut("[Warning] no breakpoint could be set; unknown filename ")
      return
  else:
    result.filename = fp.filename

proc createBreakPoint(fp: PFrame, s: cstring, start: int) =
  let br = parseBreakpoint(fp, s, start)
  if not br.filename.isNil:
    if not addBreakpoint(br.filename, br.low, br.high):
      dbgMsgOut("[Warning] no breakpoint could be set; out of breakpoint space ")

proc breakpointToggle(fp: PFrame, s: cstring, start: int) =
  var a = parseBreakpoint(fp, s, start)
  if not a.filename.isNil:
    for b in listBreakpoints():
      # compare regardless whether isActive or disabled
      if a.low >= abs(b.low) and a.low <= abs(b.high) and fileMatches(a.filename, b.filename):
        b.flip
        return
  dbgMsgOut("[Warning] unknown breakpoint ")

proc commandPrompt(fp: PFrame)        # forward declaration

proc checkForBreakpoint(fp: PFrame, srcLine: cstring): bool {.discardable.} =
  result = false
  let b = checkBreakpoints(fp.filename, fp.line)
  if b != nil:
    srcCodeLine.assign(srcLine)
    dbgShowExecutionPoint(fp)
    dbgAddHistory(fp)
    commandPrompt(fp)
    return true

proc singleCommand(cmd: cstring, dUser: var StaticStr, i: var int, again: var bool,
      dFramePtr: var PFrame, dDown: var int) =
  template `?`(x: expr): uint = hash(x)

  case hash($cmd):
  of ?"ss", ?"sys":
    dbgState = dbgStepIntoSys
    again = false
  of ?"s", ?"step":
    dbgState = dbgStepInto
    #listLocals(stdout, dFramePtr)
    again = false
  of ?"n", ?"next":
    dbgState = dbgStepOver
    again = false
  of ?"f", ?"skipcurrent":
    dbgState = dbgSkipCurrent
    again = false
  of ?"c", ?"continue", ?"r", ?"run":
    dbgState = dbgBreakpoints
    again = false
  of ?"i", ?"ignore":
    dbgState = dbgOff
    again = false
  of ?"h", ?"?", ?"help":
    dbgHelp()
  of ?"q", ?"quit":
    dbgState = dbgQuiting
    dbgAborting = true
    again = false
    quit(1) # BUGFIX: quit with error code > 0
  of ?"e", ?"expand":
    dbgToggleExpand()
  of ?"p", ?"print":
    var prevState = dbgState
    dbgState = dbgSkipCurrent
    dbgEvaluate(stdout, dUser.data, i, dFramePtr)
    dbgState = prevState
  of ?"o", ?"out":
    dbgOut(dUser.data, i, dFramePtr)
  #of ?"sf", ?"stackframe":
  #  var
  #    prevState = dbgState
  #    prevSkipFrame = dbgSkipToFrame
  #    s = srcCodeLine
  #  dbgState = dbgSkipCurrent
  #  dbgStackFrame(dUser.data, i, dFramePtr)
  #  dbgState = prevState
  #  dbgSkipToFrame = prevSkipFrame
  #  srcCodeLine.assign(s)
  of ?"w", ?"where":
    listHistory(stdout, dUser.data, i, dFramePtr)
  of ?"l", ?"locals":
    var
      prevState = dbgState
      s = srcCodeLine
    dbgState = dbgSkipCurrent
    var stream = getOptionalOutFile($dUser.data, i)
    if stream != nil:
      listLocals(stream, dFramePtr)
      close(stream)
    else:
      listLocals(stdout, dFramePtr)
    dbgState = prevState
    srcCodeLine.assign(s)
  of ?"g", ?"globals":
    var
      prevState = dbgState
      s = srcCodeLine
    dbgState = dbgSkipCurrent
    var stream = getOptionalOutFile($dUser.data, i)
    if stream != nil:
      listGlobals(stream, dFramePtr)
      close(stream)
    else:
      listGlobals(stdout, dFramePtr)
    dbgState = prevState
    srcCodeLine.assign(s)
  of ?"v", ?"variables":
    var
      prevState = dbgState
      s = srcCodeLine
    dbgState = dbgSkipCurrent
    var stream = getOptionalOutFile($dUser.data, i)
    if stream != nil:
      listVariables(stream, dFramePtr)
      close(stream)
    else:
      listVariables(stdout, dFramePtr)
    dbgState = prevState
    srcCodeLine.assign(s)
  of ?"d", ?"down":
    if dDown > 0:
      dFramePtr = framePtr
      for j in 1 .. <dDown:
        dFramePtr = dFramePtr.prev
      dec(dDown)
    dbgShowCurrentProc(dFramePtr)
  of ?"u", ?"up":
    if dFramePtr != nil:
      inc(dDown)
      dFramePtr = dFramePtr.prev
      dbgShowCurrentProc(dFramePtr)
  of ?"bt", ?"backtrace":
    dbgWriteStackTrace(dFramePtr)
  of ?"b", ?"break":
    createBreakPoint(dFramePtr, dUser.data, i)
    listBreakPoints()
  of ?"bp", ?"breakpoints":
    listBreakPoints()
  of ?"t", ?"toggle":
    breakpointToggle(dFramePtr, dUser.data, i)
    listBreakPoints()
  of ?"fn", ?"filenames":
    listFilenames()
  of ?"md", ?"maxdisplay":
    var parsed: int
    i = scanNumber(dUser.data, parsed, i)
    if dUser.data[i-1] in {'0'..'9'}:
      if parsed == 0: maxDisplayRecDepth = -1
      else: maxDisplayRecDepth = parsed
    else:
      invalidCommand()
  else:
    invalidCommand()

proc commandPrompt(fp: PFrame) =
  # User interaction
  # On return, user code executes again
  var
    again = true
    dbgFramePtr = fp # for going down and up the stack
    dbgDown = 0 # how often we did go down
    dbgTemp: StaticStr
    i = 0
  while again:
    if dbgNr <= 0:
      dbgStr.setLen(0)
      printBeg()
      print(">> ")
      let oldLen = dbgUser.len
      dbgUser.len = 0
      if not readLine(stdin, dbgUser): break
      if dbgUser.len == 0: dbgUser.len = oldLen

      # get a number then a command -> repeat command n times
      i = scanNumber(dbgUser.data, dbgNr, 0)
      if dbgNr > 0:
        if dbgNr >= MaxRep: dbgNr = MaxRep - 1
        discard scanFilename(dbgUser.data, dbgStr, i) # get the rest of input
        dbgUser.assign(dbgStr)  # save it back to dbgUser
      i = scanWord(dbgUser.data, dbgTemp, 0)
    else:
      dbgUser.assign(dbgStr)  # put repetitive cmd in dbgUser
      i = scanWord(dbgUser.data, dbgTemp, 0)

    singleCommand(dbgTemp.data, dbgUser, i, again, dbgFramePtr, dbgDown)
    dbgNr = max(0, dbgNr - 1)  #dec to zero

proc endbStep() =
  # we get into here if an unhandled exception has been raised
  # XXX: do not allow the user to run the program any further?
  # XXX: BUG: the frame is lost here!
  dbgShowExecutionPoint(framePtr)
  commandPrompt(framePtr)

# ------------------- Endb hooks  ------------------------------------
type
  SfType = enum
    sftNone,             ## frame that is not a defined SfType
    sftSameLevelFrame,   ## next code line in same proc/global scope
    sftLibFrame,         ## code line in a lib/xxxx file (not lib/system )
    sftPrevLevelFrame,   ## code line in upper level scope (proc that called current proc)
    sftNextLevelFrame    ## code line in a called proc (not a lib/xxxx file)

proc isChild(possibleParent, possibleChild: PFrame): bool =
  var pf = possibleChild
  while pf.prev != nil:
    if pf.prev == possibleParent: return true
    pf = pf.prev

proc getFrameType(sf: PFrame): SfType {.nimcall.} =
  result = sftNone
  if fileMatchesSource(sf):  # non-Lib file
    if framePtr.prev == nil:
      result = sftPrevLevelFrame
    elif dbgPrevStackFrame == framePtr:
      result = sftSameLevelFrame  # next line of code
    elif isChild(dbgPrevStackFrame, framePtr):
      result = sftNextLevelFrame  # stepping into a proc
    else:
      result = sftPrevLevelFrame
  elif not isSystemFile(sf.filename):
    result = sftLibFrame

template showExecPoint() =
  srcCodeLine.assign(srcLine)
  dbgShowExecutionPoint(framePtr)
  dbgAddHistory(framePtr)
  commandPrompt(framePtr)
  dbgPrevStackFrame = framePtr

proc lineHookImpl(srcLine: cstring) {.nimcall.} =
  var sft: SfType = sftNone
  case dbgState
  of dbgStepInto:      # the next step (non lib/xxxx)
    if not checkForBreakpoint(framePtr, srcLine):
      sft = getFrameType(framePtr)
      if sft != sftLibFrame and sft != sftNone:
        showExecPoint()
  of dbgStepIntoSys:   # not into lib/xxxx files (not lib/system)
    if not checkForBreakpoint(framePtr, srcLine):
      sft = getFrameType(framePtr)
      if sft != sftNone:
        showExecPoint()
  of dbgSkipCurrent:   # skip to next upper scope level (leave this proc)
    if not checkForBreakpoint(framePtr, srcLine):
      sft = getFrameType(framePtr)
      if sft == sftPrevLevelFrame:
        showExecPoint()
  of dbgStepOver:      # same level or up a level
    if not checkForBreakpoint(framePtr, srcLine):
      sft = getFrameType(framePtr)
      if sft == sftPrevLevelFrame or sft == sftSameLevelFrame:
        showExecPoint()
  of dbgBreakpoints:
    # debugger is only interested in breakpoints
    checkForBreakpoint(framePtr, srcLine)
  of dbgOff:
    # check but ignore breakpoints (it errors otherwise)
    let b = checkBreakpoints(framePtr.filename, framePtr.line)
  else:
    dbgMsgOut("[Warning] unknown debug state: " & $dbgState)
    discard

proc watchpointHookImpl(address: pointer, name: cstring,
                        typ: PNimType, typStr: cstring,
                        line: int) {.nimcall.} =
  dbgMsgOut("WATCHPOINT(", $line, ") ", $name, ": ",
              $typStr, " = ", dbgRepr(address, typ))
  printLnDone()

#----------------- Init -----------------------------------------

proc initDebugger {.inline.} =
  dbgState = dbgStepInto
  dbgUser.len = 1
  dbgUser.data[0] = 's'
  dbgPrevStackFrame = framePtr
  dbgWatchpointHook = watchpointHookImpl
  dbgLineHook = lineHookImpl
