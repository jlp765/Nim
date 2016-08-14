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
  EndbBeg = "*** endb| "
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
  dbgUser: StaticStr    # buffer for user input; first command is ``step_into``
                        # needs to be global cause we store the last command
                        # in it
  dbgState: DbgState    # state of debugger
  dbgSkipToFrame: PFrame # frame to be skipped to

  maxDisplayRecDepth: int = 5 # do not display too much data!

  #brkPoints: array[0..DbgLen, BreakpointFilename]

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

proc write(f: File, s: StaticStr) =
  write(f, s.data.cstring)

proc writeLine(f: File, s: StaticStr) =
  writeLine(f, s.data.cstring)

proc writeLine(f: File) =
  writeLine(f, "\n")

proc hasExt(s: cstring): bool =
  # returns true if s has a filename extension
  var i = 0
  while s[i] != '\0':
    if s[i] == '.': return true
    inc i

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

proc isSourceFile(): bool =
  # true if not a system file and is listed in
  # dbgFilenames *before* the system files.
  # imported files are *after* the system files
  var i = 0
  result = false
  while true:
    let x = dbgFilenames[i]
    if isNil(x) or x[0] == '\0':
      return false
    # if reached system files, not the source file
    if isSystemFile(x):
      return false
    if x.contain(framePtr.filename):
      return true
    inc i
  return false

# ------------------- dbgXXXXXX support ------------------------------------

proc listBreakPoints() =
  write(stdout, EndbBeg)
  writeLine(stdout, "Breakpoints:")
  for b in listBreakpoints():
    write(stdout, abs(b.low))
    if b.high != b.low:
      write(stdout, "..")
      write(stdout, abs(b.high))
    write(stdout, " ")
    write(stdout, b.filename)
    if not b.isActive:
      writeLine(stdout, " [disabled]")
    else:
      writeLine(stdout, "")
  writeLine(stdout, EndbDone)

proc openAppend(filename: cstring): File =
  var p: pointer = fopen(filename, "ab")
  if p != nil:
    result = cast[File](p)
    writeLine(result, "----------------------------------------")

proc reprNimType(typ: PNimType): string =
  case typ.kind
    of tySet: result = "Set"
    of tyArray, tyArrayConstr: result = "Array"
    of tyTuple: result = "Tuple"
    of tyObject: result = "Object"
    of tyRef: result = "Ref"
    of tyPtr: result = "Ptr"
    of tySequence: result = "Seq"
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
    of tyEnum: result = "Enum"
    of tyBool: result = "Bool"
    of tyChar: result = "Char"
    of tyString: result = "String"
    of tyCString: result = "cstring"
    of tyRange: result = "Range"
    of tyProc: result = "Proc"
    of tyPointer: result = "Pointer"
    of tyVar:
      result = "Var (size=" & $typ.size & ") "
      if typ.base != nil:
        result = result & reprNimType(typ.base)
      #result = "Var"
    else: result = "Unknown"

proc dbgRepr(p: pointer, typ: PNimType): string =
  var cl: ReprClosure
  initReprClosure(cl)
  cl.recDepth = maxDisplayRecDepth
  # locks for the GC turned out to be a bad idea...
  # inc(recGcLock)
  result = ""
  #reprAux(result, p, typ, cl)
  ## dec(recGcLock)

  var
    ppi: ptr pointer = cast[ptr pointer](p)
    pi: ptr int = cast[ptr int](ppi[])
  if typ.kind == tyVar and typ.size <= 8:
    result = $(pi[])
  else:
    reprAux(result, p, typ, cl)

  deinitReprClosure(cl)

proc writeVariable(stream: File, slot: VarSlot) =
  write(stream, slot.name)
  write(stream, " = (")
  write(stream, reprNimType(slot.typ))
  write(stream, ") ")
  writeLine(stream, dbgRepr(slot.address, slot.typ))

proc listFrame(stream: File, f: PFrame) =
  write(stream, EndbBeg)
  write(stream, "Frame (")
  write(stream, f.len)
  writeLine(stream, " slots):")
  for i in 0 .. f.len-1:
    writeLine(stream, getLocal(f, i).name)
  writeLine(stream, EndbDone)

proc listLocals(stream: File, f: PFrame) =
  write(stream, EndbBeg)
  write(stream, "Locals: (")
  write(stream, f.len)
  writeLine(stream, " slots):")
  for i in 0 .. f.len-1:
    writeVariable(stream, getLocal(f, i))
  writeLine(stream, EndbDone)

proc listGlobals(stream: File) =
  write(stream, EndbBeg)
  writeLine(stream, "Globals:")
  for i in 0 .. getGlobalLen()-1:
    writeLine(stream, getGlobal(i).name)
  writeLine(stream, EndbDone)

proc debugOut(msg: cstring) =
  # the *** *** markers are for easy recognition of debugger
  # output for external frontends.
  write(stdout, EndbBeg)
  write(stdout, msg)
  writeLine(stdout, EndbEnd)

proc dbgFatal(msg: cstring) =
  debugOut(msg)
  dbgAborting = true # the debugger wants to abort
  quit(1)

proc dbgShowCurrentProc(dbgFramePointer: PFrame) =
  if dbgFramePointer != nil:
    write(stdout, EndbBeg)
    write(stdout, "now in proc: ")
    write(stdout, dbgFramePointer.procname)
    writeLine(stdout, EndbEnd)
  else:
    write(stdout, EndbBeg)
    write(stdout, "(proc name not available) ")
    writeLine(stdout, EndbEnd)

proc dbgShowSourceLine() =
  #var srcCode: StaticStr
  #srcCode.setlen(0)
  #getSourceFileLine(srcCode, framePtr.filename, framePtr.line)
  write(stdout, " -> ")
  #write(stdout, srcCode.data.cstring)
  write(stdout, getSourceLine())
  #write(stdout, " ")

proc dbgShowExecutionPoint() =
  write(stdout, EndbBeg)
  write(stdout, framePtr.filename)
  write(stdout, "(")
  write(stdout, framePtr.line)
  write(stdout, ") ")
  write(stdout, framePtr.procname)
  dbgShowSourceLine()
  #writeLine(stdout, EndbDone)
  writeLine(stdout, "")

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
l, locals                 display available local variables
g, globals                display available global variables
sf, stackframe [file]     display current stack frame [and write it to file]
e, eval <expr>            evaluate the expression <expr> (if in local or
                          global list)
o, out <file> <expr>      evaluate <expr> and write it to <file>
w, where                  display the current execution point
u, up                     go up in the call stack
d, down                   go down in the call stack
bt, backtrace             display the entire call stack
md, maxdisplay <integer>  set the display's recursion maximum (0..9)
""")

proc invalidCommand() =
  debugOut("[Warning] invalid command ignored (type 'h' for help) ")

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

proc dbgEvaluate(stream: File, s: cstring, start: int, f: PFrame) =
  var dbgTemp: StaticStr
  var i = scanWord(s, dbgTemp, start)
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
  else:
    for i in 0 .. f.len-1:
      let v = getLocal(f, i)
      if c_strcmp(v.name, dbgTemp.data) == 0:
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

proc listFilenames() =
  write(stdout, EndbBeg)
  writeLine(stdout, "Files:")
  var i = 0
  while true:
    let x = dbgFilenames[i]
    if x.isNil: break
    writeLine(stdout, x)
    inc i
  writeLine(stdout, EndbDone)

proc dbgWriteStackTrace(f: PFrame)
proc commandPrompt() =
  # if we return from this routine, user code executes again
  var
    again = true
    dbgFramePtr = framePtr # for going down and up the stack
    dbgDown = 0 # how often we did go down
    dbgTemp: StaticStr

  while again:
    write(stdout, EndbBeg)
    write(stdout, ">>")
    let oldLen = dbgUser.len
    dbgUser.len = 0
    if not readLine(stdin, dbgUser): break
    if dbgUser.len == 0: dbgUser.len = oldLen
    # now look what we have to do:
    var i = scanWord(dbgUser.data, dbgTemp, 0)
    template `?`(x: expr): expr = dbgTemp == cstring(x)
    if ?"ss" or ?"sys":
      dbgState = dbStepIntoSys
      again = false
    elif ?"s" or ?"step":
      dbgState = dbStepInto
      again = false
    elif ?"n" or ?"next":
      dbgState = dbStepOver
      dbgSkipToFrame = framePtr
      again = false
    elif ?"f" or ?"skipcurrent":
      dbgState = dbSkipCurrent
      dbgSkipToFrame = framePtr.prev
      again = false
    elif ?"c" or ?"continue" or ?"r" or ?"run":
      dbgState = dbBreakpoints
      again = false
    elif ?"i" or ?"ignore":
      dbgState = dbOff
      again = false
    elif ?"h" or ?"help":
      dbgHelp()
    elif ?"q" or ?"quit":
      dbgState = dbQuiting
      dbgAborting = true
      again = false
      quit(1) # BUGFIX: quit with error code > 0
    elif ?"e" or ?"eval":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
      dbgState = dbSkipCurrent
      dbgEvaluate(stdout, dbgUser.data, i, dbgFramePtr)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
    elif ?"o" or ?"out":
      dbgOut(dbgUser.data, i, dbgFramePtr)
    elif ?"sf" or ?"stackframe":
      dbgStackFrame(dbgUser.data, i, dbgFramePtr)
    elif ?"w" or ?"where":
      dbgShowExecutionPoint()
    elif ?"l" or ?"locals":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
      dbgState = dbSkipCurrent
      listLocals(stdout, dbgFramePtr)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
    elif ?"g" or ?"globals":
      var
        prevState = dbgState
        prevSkipFrame = dbgSkipToFrame
      listGlobals(stdout)
      dbgState = prevState
      dbgSkipToFrame = prevSkipFrame
    elif ?"u" or ?"up":
      if dbgDown <= 0:
        debugOut("[Warning] cannot go up any further ")
      else:
        dbgFramePtr = framePtr
        for j in 0 .. dbgDown-2: # BUGFIX
          dbgFramePtr = dbgFramePtr.prev
        dec(dbgDown)
      dbgShowCurrentProc(dbgFramePtr)
    elif ?"d" or ?"down":
      if dbgFramePtr != nil:
        inc(dbgDown)
        dbgFramePtr = dbgFramePtr.prev
        dbgShowCurrentProc(dbgFramePtr)
      else:
        debugOut("[Warning] cannot go down any further ")
    elif ?"bt" or ?"backtrace":
      dbgWriteStackTrace(framePtr)
    elif ?"b" or ?"break":
      createBreakPoint(dbgUser.data, i)
      listBreakPoints()
    elif ?"bp" or ?"breakpoints":
      listBreakPoints()
    elif ?"t" or ?"toggle":
      breakpointToggle(dbgUser.data, i)
      listBreakPoints()
    elif ?"fn" or ?"filenames":
      listFilenames()
    elif ?"md" or ?"maxdisplay":
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
      write(stdout, "(")
      write(stdout, skipped)
      writeLine(stdout, " calls omitted) ...")
    else:
      write(stdout, tempFrames[j].filename)
      if tempFrames[j].line > 0:
        write(stdout, '(')
        write(stdout, tempFrames[j].line)
        write(stdout, ')')
      write(stdout, ' ')
      writeLine(stdout, tempFrames[j].procname)

proc checkForBreakpoint: bool {.discardable.} =
  result = false
  #if not fileMatchesSource(): getSourceFileData(framePtr.filename)
  let b = checkBreakpoints(framePtr.filename, framePtr.line)
  if b != nil:
    write(stdout, EndbBeg)
    write(stdout, "BREAKPOINT")
    writeLine(stdout, EndbEnd)
    dbgShowExecutionPoint()
    commandPrompt()
    return true

# ------------------- Endb hooks  ------------------------------------

proc lineHookImpl() {.nimcall.} =
  case dbgState
  of dbStepInto:
    if not checkForBreakpoint():
      if fileMatchesSource():
        # we really want the command prompt here:
        dbgShowExecutionPoint()
        commandPrompt()
  of dbStepIntoSys:
    if not checkForBreakpoint():
      # we really want the command prompt here:
      dbgShowExecutionPoint()
      commandPrompt()
  of dbSkipCurrent:  # skip to next non-system routine
    if framePtr == dbgSkipToFrame:
      dbgShowExecutionPoint()
      commandPrompt()
    else:
      # breakpoints are wanted though (I guess)
      checkForBreakpoint()
  of dbStepOver: # skip to next line (over a routine call) in current source file
    # skip over routine calls in same file
    # if in imported file, and no next line or routine to skip,
    #   then stop at next source file line
    if framePtr == dbgSkipToFrame:
      dbgShowExecutionPoint()
      commandPrompt()
    else:
      # breakpoints are wanted though (I guess)
      checkForBreakpoint()
  of dbBreakpoints:
    # debugger is only interested in breakpoints
    checkForBreakpoint()
  of dbOff:
    # check but ignore breakpoints (it errors otherwise)
    let b = checkBreakpoints(framePtr.filename, framePtr.line)
  else:
    debugOut("[Warning] unknown debug state: " & $dbgState)
    discard

proc watchpointHookImpl(name: cstring) {.nimcall.} =
  write(stdout, EndbBeg)
  write(stdout, "WATCHPOINT ")
  write(stdout, name)
  write(stdout, " ")
  #dbgWriteStackTrace(framePtr)
  for i in 0 .. <framePtr.len:
    let v = getLocal(framePtr, i)
    if c_strcmp(v.name, name) == 0:
      writeVariable(stdout, v)
  writeLine(stdout, EndbDone)

proc initDebugger {.inline.} =
  dbgState = dbStepInto
  dbgUser.len = 1
  dbgUser.data[0] = 's'
  dbgWatchpointHook = watchpointHookImpl
  dbgLineHook = lineHookImpl
