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
  #EndbBeg = "*** endb| "
  EndbBeg = "endb| "
  EndbEnd = " ***"            # use writeLine(stream, EndbEnd)
  EndbDone = "***"            # after prev writeLine

  sysStr = "system".cstring
  StaticStrLen = 100          # StaticStr data size
  DbgLen = 127                # endb dbg break points

type
  StaticStr = object
    len: int
    data: array[0..StaticStrLen, char]

  BreakpointFilename = object
    b: ptr Breakpoint
    filename: StaticStr

  DbgState = enum
    dbOff,          # debugger is turned off
    dbStepInto,     # debugger is in tracing mode
    dbStepIntoSys,  # debugger traces into system files
    dbStepOver,     # step over proc calls
    dbSkipCurrent,  # skip ahead until finished current proc or level
    dbQuiting,      # debugger wants to quit
    dbBreakpoints   # debugger is only interested in breakpoints

{.deprecated: [TStaticStr: StaticStr, TBreakpointFilename: BreakpointFilename,
              TDbgState: DbgState].}

var
  dbgUser: StaticStr          # buffer for user input; first command is ``step_into``
                              # needs to be global cause we store the last command
                              # in it
  dbgState: DbgState          # state of debugger
  dbgExpand: bool = false     # show values when list local or global variables
  dbgSkipToFrame: PFrame      # frame to be skipped to

  maxDisplayRecDepth: int = 5 # do not display too much data!

  #brkPoints: array[0..DbgLen, BreakpointFilename]
  srcCodeLine: StaticStr

#-------------------- Support procs ----------------------------------------

proc hash(x: string): uint =
  var h: uint = 0
  for i in 0 .. x.len-1:
    h = h + ord(x[i]).uint
  result = h

# ------------------- StaticStr support ------------------------------------

proc setLen(s: var StaticStr, newLen=0) =
  s.len = newLen
  s.data[newLen] = '\0'

proc add(s: var StaticStr, c: char) =
  if s.len < high(s.data)-1:
    s.data[s.len] = c
    s.data[s.len+1] = '\0'
    inc s.len

proc add(s: var StaticStr, c: cstring) =
  var i = 0
  while c[i] != '\0':
    add s, c[i]
    inc i

proc assign(s: var StaticStr, c: cstring) =
  setLen(s)
  add s, c

proc assign(s: var StaticStr, c: StaticStr) =
  setLen(s)
  add s, cstring(c.data)

proc `==`(a, b: StaticStr): bool =
  if a.len == b.len:
    for i in 0 .. a.len-1:
      if a.data[i] != b.data[i]: return false
    return true
proc `!=`(a, b: StaticStr): bool {.inline.} =
  return not (a == b)

proc `==`(a: StaticStr, b: cstring): bool =
  result = c_strcmp(a.data, b) == 0

proc `!=`(a: StaticStr, b: cstring): bool {.inline.} =
  return not (a == b)

proc `contain`(a, b: cstring): bool =
  var
    st = 0
    bLen = b.len - 1
    mtch = 0
  result = false
  for i in 0 .. a.len-1:
    if st == 1 and a[i] == b[mtch] and mtch == bLen:
      return true
    if st == 1 and a[i] != b[mtch]:
      st = 0    # keep trying
      mtch = 0
      continue
    if st == 1 and a[i] == b[mtch]:
      inc mtch
      continue
    if st == 0 and a[i] == b[mtch]:
      inc st    # first match
      inc mtch
      continue

proc hasExt(s: cstring): bool =
  # returns true if s has a filename extension
  var i = 0
  while s[i] != '\0':
    if s[i] == '.': return true
    inc i

proc write(f: File, s: StaticStr) =
  write(f, s.data.cstring)

proc writeLine(f: File, s: StaticStr) =
  writeLine(f, s.data.cstring)

proc writeLine(f: File) =
  writeLine(f, "\n")

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
  stdout.write(s.data.cstring)
proc print[T: string|cstring](s: T) =
  stdout.write(s)

proc printLn() =
  stdout.writeLine("")
proc printLn(s: StaticStr) =
  stdout.writeLine(s.data.cstring)
proc printLn[T: string|cstring](s: T) =
  stdout.writeLine(s)

proc print(i: int) =
  stdout.write(i)

proc printLn(i: int) =
  stdout.writeLine(i)

proc printBeg()  =
  stdout.write(EndbBeg)
proc printBeg(stream: File) =
  stream.write(EndbBeg)

proc printLnBeg() =
  stdout.writeLine(EndbBeg)
proc printLnBeg(stream: File) =
  stream.writeLine(EndbBeg)

proc printEnd() {.inline.} =
  #stdout.write(EndbEnd)
  discard
proc printEnd(stream: File) {.inline.} =
  #stream.write(EndbEnd)
  discard

proc printLnEnd() {.inline.} =
  #stdout.writeLine(EndbEnd)
  stdout.writeLine("")
proc printLnEnd(stream: File) {.inline.} =
  #stream.writeLine(EndbEnd)
  stream.writeLine("")

proc printLnDone() {.inline.} =
  #stdout.writeLine(EndbDone)
  discard
proc printLnDone(stream: File) {.inline.} =
  #stream.writeLine(EndbDone)
  discard

# ------------------- StaticStr scanning support ---------------------

proc scanAndAppendWord(src: cstring, a: var StaticStr, start: int): int =
  result = start
  # skip whitespace:
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of 'a'..'z', '0'..'9': add(a, src[result])
    of '_': discard # just skip it
    of 'A'..'Z': add(a, chr(ord(src[result]) - ord('A') + ord('a')))
    else: break
    inc(result)

proc scanWord(src: cstring, a: var StaticStr, start: int): int =
  setlen(a)
  result = scanAndAppendWord(src, a, start)

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

proc scanNumber(src: cstring, a: var int, start: int): int =
  result = start
  a = 0
  while src[result] in {'\t', ' '}: inc(result)
  while true:
    case src[result]
    of '0'..'9': a = a * 10 + ord(src[result]) - ord('0')
    of '_': discard # skip underscores (nice for long line numbers)
    else: break
    inc(result)

# ------------------- sourceFile support ------------------------------------

proc isSystemFile(fn: cstring): bool {.inline.} =
  result = contain(fn, sysStr)

proc fileMatchesSource(): bool =
  # true if framePtr.filename matches
  #    the current sourceFile.filename
  # true if not a system file
  var i = 0
  result = false
  while true:
    let x = dbgFilenames[i]
    if isNil(x) or x[0] == '\0': break
    if contain(x, sysStr):
      inc i
      continue
    if x.contain(framePtr.filename):
      result = true
      break
    inc i

proc getCurrentFileNameNoExt(): cstring =
  var t: StaticStr
  for c in framePtr.filename:
    if c == '.': break
    t.add(c)
  result = t.data.cstring

#----------------------- top level actions --------------------------

proc dbgRepr(p: pointer, typ: PNimType): string       # fwd decl

proc reprNimType(typ: PNimType): string =
  case typ.kind
    of tySet: result = "set"
    of tyArray, tyArrayConstr: result = "array"
    of tyTuple: result = "tuple"
    of tyObject: result = "object"
    of tyRef: result = "ref"
    of tyPtr: result = "ptr"
    of tySequence: result = "seq"
    of tyInt: result = "int"
    of tyInt8: result = "int8"
    of tyInt16: result = "int16"
    of tyInt32: result = "int32"
    of tyInt64: result = "int64"
    of tyUInt8: result = "uint8"
    of tyUInt16: result = "uint16"
    of tyUInt32: result = "uint32"
    of tyUInt64: result = "uint"
    of tyFloat: result = "float"
    of tyFloat32: result = "float32"
    of tyFloat64: result = "float64"
    of tyEnum: result = "enum"
    of tyBool: result = "bool"
    of tyChar: result = "char"
    of tyString: result = "string"
    of tyCString: result = "cstring"
    of tyRange: result = "range"
    of tyProc: result = "proc"
    of tyPointer: result = "pointer"
    of tyVar:
      result = "var (size=" & $typ.size & ") "
      if typ.base != nil:
        result = result & reprNimType(typ.base)
      #result = "var"
    else: result = "unknown"

proc writeVariable(stream: File, slot: VarSlot) =
  writeLine(stream, $slot.name & ": " & reprNimType(slot.typ) & " = " & dbgRepr(slot.address, slot.typ))

proc listFrame(stream: File, f: PFrame) =
  #printBeg(stream)
  writeLine(stream, "Frame (" & $f.len & " slots):")
  for i in 0 .. f.len-1:
    if dbgExpand:
      writeVariable(stream, getLocal(f, i))
    else:
      writeLine(stream, getLocal(f, i).name)
  printLnDone(stream)

proc listLocals(stream: File, f: PFrame) =
  #printBeg(stream)
  writeLine(stream, "Locals: (" & $f.len & " slots):")
  for i in 0 .. f.len-1:
    if dbgExpand:
      writeVariable(stream, getLocal(f, i))
    else:
      writeLine(stream, getLocal(f, i).name)
  printLnDone(stream)

proc listGlobals(stream: File) =
  #printBeg(stream)
  writeLine(stream, "Globals:")
  for i in 0 .. getGlobalLen()-1:
    if dbgExpand:
      writeVariable(stream, getGlobal(i))
    else:
      writeLine(stream, getGlobal(i).name)
  printLnDone(stream)

proc listBreakPoints() =
  #printBeg()
  printLn("Breakpoints:")
  for b in listBreakpoints():
    print($abs(b.low))
    if b.high != b.low:
      print(".." & $abs(b.high))
    print(" ")
    print(b.filename)
    if not b.isActive:
      printLn(" [disabled]")
    else:
      printLn()
  printLnDone()

proc openAppend(filename: cstring): File =
  var p: pointer = fopen(filename, "ab")
  if p != nil:
    result = cast[File](p)
    writeLine(result, "----------------------------------------")

proc debugOut(msg: cstring) {.inline.} =
  # the *** *** markers are for easy recognition of debugger
  # output for external frontends.
  printBeg()
  print(msg)
  printLnEnd()

proc listFilenames() =
  printEnd()
  printLn("Files:")
  var i = 0
  while true:
    let x = dbgFilenames[i]
    if x.isNil: break
    printLn(x)
    inc i
  printLnDone()

proc invalidCommand() {.inline.} =
  debugOut("[Warning] invalid command ignored (type 'h' for help) ")

# ------------------- dbgXXXXXX support ------------------------------------

proc dbgRepr(p: pointer, typ: PNimType): string =
  var cl: ReprClosure
  initReprClosure(cl)
  cl.recDepth = maxDisplayRecDepth
  result = ""
  reprAux(result, p, typ, cl)
  #
  #var
  #  ppi: ptr pointer = cast[ptr pointer](p)
  #  pi: ptr int = cast[ptr int](ppi[])
  #if typ.kind == tyVar and typ.size <= 8:
  #  result = $(pi[])
  #else:
  #  reprAux(result, p, typ, cl)

  deinitReprClosure(cl)

proc dbgFatal(msg: cstring) =
  debugOut(msg)
  dbgAborting = true         # the debugger wants to abort
  quit(1)

proc dbgShowCurrentProc(dbgFramePointer: PFrame) =
  if dbgFramePointer != nil:
    printBeg()
    print("now in proc: " & $dbgFramePointer.procname)
    printLnEnd()
  else:
    printBeg()
    print("(proc name not available) ")
    printLnEnd()

proc dbgShowExecutionPoint() {.noinline.} =
  # don't combine these print statements
  printBeg()
  print(framePtr.filename)
  print("(")
  print(framePtr.line)
  print(") ")
  print(framePtr.procname)
  print(" -> ")
  print(srcCodeLine)
  printLn()

proc dbgHelp() =
  debugOut("""
list of commands (see the manual for further help):
--------------------- GENERAL -----------------------------
h, help                   display this help message
q, quit                   quit the debugger and the program
<ENTER>                   repeat the previous debugger command
--------------------- EXECUTING ---------------------------
s, step                   single step, to next source code line or into a proc
ss, sys                   system step, stepping into system routines
                          ss to keep at system level; f, s or n to return to
                          the upper level
n, next                   next step, stepping over proc calls
f, skipcurrent            continue execution until the current routine finishes
c, continue, r, run       continue execution until the next breakpoint
i, ignore                 continue execution, ignore all breakpoints
--------------------- BREAKPOINTS ------------------------
b, break [fromline [toline]] [file]
                          set a new breakpoint for line and file
                          if line or file are omitted the current one is used
bp, breakpoints           display the entire breakpoint list
fn, filenames             list all valid filenames
t, toggle <fromline> [file]
                          enable or disable a breakpoint
                          [file] does not require the full path or extension
--------------------- DATA DISPLAY ------------------------
e, expand                 toggle showing values (for local or global variables)
                          disable there are large objects or sequences
                          (default is off)
g, globals                display available global variables
                          which are vars from global scope or a shared lib
l, locals                 display available local variables
sf, stackframe [file]     display current stack frame [and write it to file]
p, print <expr>           print the value of <expr> (if in local or
                          global list)
o, out <file> <expr>      evaluate <expr> and write it to <file>
w, where                  display the current execution point
u, up                     go up in the call stack
d, down                   go down in the call stack
bt, backtrace             display the entire call stack
md, maxdisplay <integer>  set the display's recursion maximum (0..9)
""")

proc dbgEvaluate(stream: File, s: cstring, start: int, f: PFrame) =
  var
    dbgTemp: StaticStr
    i = scanWord(s, dbgTemp, start)
    wasFound = false
  while s[i] in {' ', '\t'}: inc(i)
  var v: VarSlot
  if s[i] == '.':
    inc(i)
    add(dbgTemp, '.')
    i = scanAndAppendWord(s, dbgTemp, i)
    for i in 0 .. getGlobalLen()-1:
      let v = getGlobal(i)
      if c_strcmp(v.name, dbgTemp.data) == 0:
        writeVariable(stream, v)
        wasFound = true
        break
  else:
    for i in 0 .. f.len-1:
      let v = getLocal(f, i)
      if c_strcmp(v.name, dbgTemp.data) == 0:
        writeVariable(stream, v)
        wasFound = true
        break
  # try prepending source filename
  var tStr: StaticStr
  if not wasFound:
    tStr.assign(getCurrentFileNameNoExt())
    tStr.add('.')
    tStr.add(dbgTemp.data)
    for i in 0 .. getGlobalLen()-1:
      let v = getGlobal(i)
      if c_strcmp(v.name, tStr.data.cstring) == 0:
        writeVariable(stream, v)

proc dbgOut(s: cstring, start: int, currFrame: PFrame) =
  var dbgTemp: StaticStr
  var i = scanFilename(s, dbgTemp, start)
  if dbgTemp.len == 0:
    invalidCommand()
    return
  var stream = openAppend(dbgTemp.data)
  if stream == nil:
    debugOut("[Warning] could not open or create file ")
    return
  dbgEvaluate(stream, s, i, currFrame)
  close(stream)

proc dbgStackFrame(s: cstring, start: int, currFrame: PFrame) =
  var dbgTemp: StaticStr
  var i = scanFilename(s, dbgTemp, start)
  if dbgTemp.len == 0:
    # just write it to stdout:
    listFrame(stdout, currFrame)
  else:
    var stream = openAppend(dbgTemp.data)
    if stream == nil:
      debugOut("[Warning] could not open or create file ")
      return
    listFrame(stream, currFrame)
    close(stream)

proc dbgWriteStackTrace(f: PFrame) =
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
      printLn("(" & $skipped & " calls omitted) ...")
    else:
      print(tempFrames[j].filename)
      if tempFrames[j].line > 0:
        print("(")
        print(tempFrames[j].line)
        print(")")
      print(" ")
      printLn(tempFrames[j].procname)

proc dbgToggleExpand() =
  # enable/disable evaluating variable when listed (local, global, stack frame)
  dbgExpand = not dbgExpand
  printLn(if dbgExpand: "variable expanding: [enabled]" else: "variable expanding: [disabled]")

#----------------- BreakPoint internals --------------------------------------------
proc parseBreakpoint(s: cstring, start: int): Breakpoint =
  var dbgTemp: StaticStr
  var i = scanNumber(s, result.low, start)
  if result.low == 0: result.low = framePtr.line
  i = scanNumber(s, result.high, i)
  if result.high == 0: result.high = result.low
  i = scanFilename(s, dbgTemp, i)
  if dbgTemp.len != 0:
    if not hasExt(dbgTemp.data): add(dbgTemp, ".nim")
    result.filename = canonFilename(dbgTemp.data.cstring)
    if result.filename.isNil:
      debugOut("[Warning] no breakpoint could be set; unknown filename ")
      return
  else:
    result.filename = framePtr.filename

proc createBreakPoint(s: cstring, start: int) =
  let br = parseBreakpoint(s, start)
  if not br.filename.isNil:
    if not addBreakpoint(br.filename, br.low, br.high):
      debugOut("[Warning] no breakpoint could be set; out of breakpoint space ")

proc breakpointToggle(s: cstring, start: int) =
  var a = parseBreakpoint(s, start)
  if not a.filename.isNil:
    for b in listBreakpoints():
      # compare regardless whether isActive or disabled
      if a.low >= abs(b.low) and a.low <= abs(b.high) and fileMatches(a.filename, b.filename):
        b.flip
        return
  debugOut("[Warning] unknown breakpoint ")

proc commandPrompt()          # forward declaration

proc checkForBreakpoint(srcLine: cstring): bool {.discardable.} =
  result = false
  let b = checkBreakpoints(framePtr.filename, framePtr.line)
  if b != nil:
    printBeg()
    print("BREAKPOINT")
    printLnEnd()
    srcCodeLine.assign(srcLine)
    dbgShowExecutionPoint()
    commandPrompt()
    return true

proc commandPrompt() =
  # User interaction proc
  # if we return from this routine, user code executes again
  var
    again = true
    dbgFramePtr = framePtr # for going down and up the stack
    dbgDown = 0 # how often we did go down
    dbgTemp: StaticStr

  while again:
    printBeg()
    print(">> ")
    let oldLen = dbgUser.len
    dbgUser.len = 0
    if not readLine(stdin, dbgUser): break
    if dbgUser.len == 0: dbgUser.len = oldLen
    var i = scanWord(dbgUser.data, dbgTemp, 0)
    template `?`(x: expr): uint = hash(x)

    case hash($dbgTemp.data):
    of ?"ss", ?"sys":
      dbgState = dbStepIntoSys
      again = false
    of ?"s", ?"step":
      dbgState = dbStepInto
      again = false
    of ?"n", ?"next":
      dbgState = dbStepOver
      dbgSkipToFrame = framePtr
      again = false
    of ?"f", ?"skipcurrent":
      dbgState = dbSkipCurrent
      dbgSkipToFrame = framePtr.prev
      again = false
    of ?"c", ?"continue", ?"r", ?"run":
      dbgState = dbBreakpoints
      again = false
    of ?"i", ?"ignore":
      dbgState = dbOff
      again = false
    of ?"h", ?"help":
      dbgHelp()
    of ?"q", ?"quit":
      dbgState = dbQuiting
      dbgAborting = true
      again = false
      quit(1) # BUGFIX: quit with error code > 0
    of ?"e", ?"eval":
      dbgToggleExpand()
    of ?"p", ?"print":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
      dbgState = dbSkipCurrent
      dbgEvaluate(stdout, dbgUser.data, i, dbgFramePtr)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
    of ?"o", ?"out":
      dbgOut(dbgUser.data, i, dbgFramePtr)
    of ?"sf", ?"stackframe":
      dbgStackFrame(dbgUser.data, i, dbgFramePtr)
    of ?"w", ?"where":
      dbgShowExecutionPoint()
    of ?"l", ?"locals":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
        s = srcCodeLine
      dbgState = dbSkipCurrent
      listLocals(stdout, dbgFramePtr)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
      srcCodeLine.assign(s)
    of ?"g", ?"globals":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
        s = srcCodeLine
      listGlobals(stdout)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
      srcCodeLine.assign(s)
    of ?"u", ?"up":
      if dbgDown <= 0:
        debugOut("[Warning] cannot go up any further ")
      else:
        dbgFramePtr = framePtr
        for j in 0 .. dbgDown-2: # BUGFIX
          dbgFramePtr = dbgFramePtr.prev
        dec(dbgDown)
      dbgShowCurrentProc(dbgFramePtr)
    of ?"d", ?"down":
      if dbgFramePtr != nil:
        inc(dbgDown)
        dbgFramePtr = dbgFramePtr.prev
        dbgShowCurrentProc(dbgFramePtr)
      else:
        debugOut("[Warning] cannot go down any further ")
    of ?"bt", ?"backtrace":
      dbgWriteStackTrace(framePtr)
    of ?"b", ?"break":
      createBreakPoint(dbgUser.data, i)
      listBreakPoints()
    of ?"bp", ?"breakpoints":
      listBreakPoints()
    of ?"t", ?"toggle":
      breakpointToggle(dbgUser.data, i)
      listBreakPoints()
    of ?"fn", ?"filenames":
      listFilenames()
    of ?"md", ?"maxdisplay":
      var parsed: int
      i = scanNumber(dbgUser.data, parsed, i)
      if dbgUser.data[i-1] in {'0'..'9'}:
        if parsed == 0: maxDisplayRecDepth = -1
        else: maxDisplayRecDepth = parsed
      else:
        invalidCommand()
    else: invalidCommand()

proc endbStep() =
  # we get into here if an unhandled exception has been raised
  # XXX: do not allow the user to run the program any further?
  # XXX: BUG: the frame is lost here!
  dbgShowExecutionPoint()
  commandPrompt()


# ------------------- Endb hooks  ------------------------------------

proc lineHookImpl(srcLine: cstring) {.nimcall.} =
  case dbgState
  of dbStepInto:
    if not checkForBreakpoint(srcLine):
      if fileMatchesSource():
        # we really want the command prompt here:
        srcCodeLine.assign(srcLine)
        dbgShowExecutionPoint()
        commandPrompt()
  of dbStepIntoSys:
    if not checkForBreakpoint(srcLine):
      # we really want the command prompt here:
      srcCodeLine.assign(srcLine)
      dbgShowExecutionPoint()
      commandPrompt()
  of dbSkipCurrent:  # skip to next non-system routine
    if framePtr == dbgSkipToFrame:
      srcCodeLine.assign(srcLine)
      dbgShowExecutionPoint()
      commandPrompt()
    else:
      # breakpoints are wanted though (I guess)
      checkForBreakpoint(srcLine)
  of dbStepOver: # skip to next line (over a routine call) in current source file
    if framePtr == dbgSkipToFrame:
      srcCodeLine.assign(srcLine)
      dbgShowExecutionPoint()
      commandPrompt()
    else:
      # breakpoints are wanted though (I guess)
      checkForBreakpoint(srcLine)
  of dbBreakpoints:
    # debugger is only interested in breakpoints
    checkForBreakpoint(srcLine)
  of dbOff:
    # check but ignore breakpoints (it errors otherwise)
    let b = checkBreakpoints(framePtr.filename, framePtr.line)
  else:
    debugOut("[Warning] unknown debug state: " & $dbgState)
    discard

proc watchpointHookImpl(name: cstring) {.nimcall.} =
  printBeg()
  print("WATCHPOINT ")
  print(name)
  print(" ")
  for i in 0 .. <framePtr.len:
    let v = getLocal(framePtr, i)
    if c_strcmp(v.name, name) == 0:
      writeVariable(stdout, v)
  printLnDone()

#----------------- Init -----------------------------------------

proc initDebugger {.inline.} =
  dbgState = dbStepInto
  dbgUser.len = 1
  dbgUser.data[0] = 's'
  dbgWatchpointHook = watchpointHookImpl
  dbgLineHook = lineHookImpl
