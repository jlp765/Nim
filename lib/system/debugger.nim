#
#
#            Nim's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This file implements basic features for any debugger.
##
## When a project is compiled with ``--debugger:endb``, a number
## of callbacks are inserted into the compiled code, which register
## debugging information with the endb debugger.
##
## ``dbgRegisterWatchpoint()`` registers a watched variable
## ``dbgRegisterGlobal()`` registers global variables (the ``VarSlot`` object)
## ``dbgRegisterType()`` registers type information for modules (the ``TypSlot`` object)
## ``dbgRegisterVariable()`` registers variables at their declaration point
## in the code (the ``VarTypSlot`` object)
##
## Global and Type callbacks occur during code initialization, but Variable callbacks
## occur as the code is stepped/run

const
  SlotSize = 1_000
  VarSize = 1_000
  BreakPointSize = 127
  WatchPointSize = 127
  FilenameSize = 127

  FileSystemCaseInsensitive = defined(windows) or defined(dos) or defined(os2)

  StaticStrLen = 100          # StaticStr data size

  sysStr = ["lib\\system\\".cstring,    # any files in this directory excluded from debugging
            "lib\\system.nim".cstring]  # also exclude system.nim

type
  StaticStr = object
    len*: int
    data*: array[0..StaticStrLen, char]

  VarSlot* {.compilerproc, final.} = object ## a slot in a frame
    address*: pointer ## the variable's address
    typ*: PNimType    ## the variable's type
    name*: cstring    ## the variable's name; for globals this is "module.name"
    typStr*: cstring  ## the variable type representation string
    lineNr*: int      ## line defined or current line

  PExtendedFrame = ptr ExtendedFrame
  ExtendedFrame = object  # If the debugger is enabled the compiler
                           # provides an extended frame. Of course
                           # only slots that are
                           # needed are allocated and not SlotSize,
                           # except for the global data description.
    f*: TFrame
    slots*: array[0..SlotSize, VarSlot]
    len*: int               # slots len

  VarTypSlot* = object       ## Static string to store cstring values,
                             ## because Registration is temporary.
    typeName*: StaticStr     ## type name  (eg "NTI_....")
    varName*: StaticStr      ## variable name (eg "modulename.varname")
    lineNr*: int             ## line nr of variable definition
    typeNameStr*: StaticStr  ## from TypeSlot typeName ("int", "string", ...)
    address*: pointer
    typ*: PNimType

  VarTypFrame = object
    vlen: int
    vars: array[0..VarSize, VarTypSlot]

  #-- break points ---
  Breakpoint* = object   ## represents a break point
    low*, high*: int     ## range from low to high; if disabled
                         ## both low and high are set to their negative values
    filename*: cstring   ## the filename of the breakpoint

  #-- watch points ---
  Hash = int
  Watchpoint* {.pure, final.} = object
    name*: cstring        ## string identifier in the watchpoint pragma
    address*: pointer
    typ*: PNimType
    typStr*: cstring
    oldValue*: Hash       ## hashed identification of watchpoint (check for value change)

{.deprecated: [TVarSlot: VarSlot, TExtendedFrame: ExtendedFrame].}
{.deprecated: [THash: Hash, TWatchpoint: Watchpoint].}

var
  dbgGlobalData*: ExtendedFrame  # this reserves much space, but
                                 # for now it is the most practical way

  dbgVarData*: VarTypFrame  # each unique variable has a type defined

  dbgFilenames*: array[0..FilenameSize, cstring] ## registered filenames;
                                        ## 'nil' terminated
  dbgFilenameLen*: int = 0
  dbgBP*: array[0..BreakPointSize, Breakpoint] # breakpoints
  dbgBPLen*: int = 0
  dbgBPbloom*: int64  # we use a bloom filter to speed up breakpoint checking

  watchpoints*: array[0..WatchPointSize, Watchpoint]
  watchpointsLen*: int = 0

  dbgLineHook*: proc (srcLine: cstring) {.nimcall.}
    ## set this variable to provide a procedure that should be called before
    ## each executed instruction. This should only be used by debuggers!
    ## Only code compiled with the ``debugger:on`` switch calls this hook.

  dbgWatchpointHook*: proc (watchpointName: pointer, name: cstring,
                              typ: PNimType, typStr: cstring,
                              line: int) {.nimcall.}

# ------------------- StaticStr support ------------------------------------

proc setLen*(s: var StaticStr, newLen=0) =
  s.len = newLen
  s.data[newLen] = '\0'

proc add*(s: var StaticStr, c: char) =
  if s.len < high(s.data)-1:
    s.data[s.len] = c
    s.data[s.len+1] = '\0'
    inc s.len

proc add*(s: var StaticStr, c: cstring) =
  var i = 0
  while c[i] != '\0':
    add s, c[i]
    inc i

proc add*(s: var StaticStr, ss: StaticStr) {.inline.} =
  s.add(ss.data)

proc assign*(s: var StaticStr, c: cstring) =
  s.setLen(0)
  add s, c

proc assign*(s: var StaticStr, c: StaticStr) =
  setLen(s)
  add s, cstring(c.data)

proc `==`*(a, b: StaticStr): bool =
  if a.len == b.len:
    return c_strcmp($a.data, $b.data) == 0
    #for i in 0 .. a.len-1:
    #  if a.data[i] != b.data[i]: return false
    #return true

proc `!=`*(a, b: StaticStr): bool {.inline.} =
  return not (a == b)

proc `==`*(a: StaticStr, b: cstring): bool =
  result = c_strcmp($a.data, b) == 0

proc `!=`*(a: StaticStr, b: cstring): bool {.inline.} =
  return not (a == b)

proc `==`*(a, b: cstring): bool =
  result = c_strcmp(a, b) == 0

#----- utility procs ---------------

proc startsWith*(s, prefix: cstring): bool =
  var i = 0
  if prefix.len > s.len: return false
  while true:
    if prefix[i] == '\0': return true
    if s[i] != prefix[i]: return false
    inc(i)

proc contains(a, b: cstring): bool =
  var
    bLen = b.len - 1
    match = 0
  result = false
  if bLen < 0: return
  for i in 0 .. a.len-1:
    if (a[i] == b[match]):
      if match == bLen: return true
      inc match
    else:
      match = 0

proc findFirst(a, b: cstring): int =
  # returns -1 if not found, else index of first b in a
  var
    bLen = b.len - 1
    match = 0
  result = -1
  if (a.len == 0) or (bLen < 0): return
  for i in 0 .. a.len-1:
    if (a[i] == b[match]):
      if match == bLen: return (i - bLen)
      inc match
    else:
      match = 0

proc fileMatches(c, bp: cstring): bool =
  # bp = breakpoint filename
  # c = current filename
  # We consider it a match if bp is a suffix of c
  # and the character for the suffix does not exist or
  # is one of: \  /  :
  # depending on the OS case does not matter!
  var blen: int = bp.len
  var clen: int = c.len
  if blen > clen: return false
  # check for \ /  :
  if clen-blen-1 >= 0 and c[clen-blen-1] notin {'\\', '/', ':'}:
    return false
  var i = 0
  while i < blen:
    var x = bp[i]
    var y = c[i+clen-blen]
    when FileSystemCaseInsensitive:
      if x >= 'A' and x <= 'Z': x = chr(ord(x) - ord('A') + ord('a'))
      if y >= 'A' and y <= 'Z': y = chr(ord(y) - ord('A') + ord('a'))
    if x != y: return false
    inc(i)
  return true

proc canonFilename*(filename: cstring): cstring =
  ## returns 'nil' if the filename cannot be found.
  for i in 0 .. <dbgFilenameLen:
    result = dbgFilenames[i]
    if fileMatches(result, filename): return result
  result = nil

#----------- Compiler procs ------------------------------

proc dbgRegisterFilename(filename: cstring) {.compilerproc.} =
  # XXX we could check for duplicates here for DLL support
  var found = false
  if filename.contains(" "): return
  for s in sysStr:
    if filename.startsWith(s):
      found = true
      break
  if not found:
    dbgFilenames[dbgFilenameLen] = filename
    inc dbgFilenameLen

proc dbgRegisterBreakpoint(line: int,
                           filename, name: cstring) {.compilerproc.} =
  let x = dbgBPLen
  if x >= high(dbgBP):
    write(stdout, "[Warning] exceeded breakpoint limit")
    return
  inc(dbgBPLen)
  dbgBP[x].filename = filename
  dbgBP[x].low = line
  dbgBP[x].high = line
  dbgBPbloom = dbgBPbloom or line

proc genericHash(dest: pointer, mt: PNimType): int

proc dbgRegisterWatchpoint(address: pointer, name: cstring,
                           typ: PNimType, typStr: cstring,
                           line: int, typNameStr: cstring) {.compilerproc.} =
  # This is called as the debugger steps/runs thru the code.
  # The line allows the same variable to be watched at multiple
  # places (each a unique line number)
  let L = watchPointsLen
  let h = genericHash(address, typ)
  for i in 0.. <L:
    if watchPoints[i].address == address and
        $watchPoints[i].name == $name:
      # address may have changed
      if h != watchPoints[i].oldValue:
        dbgWatchpointHook(address, name, typ, typNameStr, line)
        watchPoints[i].typ = typ
        watchPoints[i].typStr = typNameStr
        watchPoints[i].oldValue = h
      return
  dbgWatchpointHook(address, name, typ, typNameStr, line)
  if L >= watchPoints.high:
    write(stdout, "[Warning] cannot register watchpoint")
    return
  watchPoints[L].name = name
  watchPoints[L].address = address
  watchPoints[L].typ = typ
  watchPoints[L].typStr = typNameStr
  watchPoints[L].oldValue = h
  inc watchPointsLen

proc dbgRegisterGlobal(line: int, name: cstring, address: pointer,
                       typ: PNimType, typStr: cstring) {.compilerproc.} =
  # line:       22
  # name:       myfile.myvar
  # address:    <pointer>      (&myvar_dHVT79bqJtcBwB9cBvt9b89csA)
  # typ:        <pointer>      (&NTI_77mFvmsOLKik79ci2hXkHEg_)
  let i = dbgGlobalData.len
  if i >= high(dbgGlobalData.slots):
    #debugOut("[Warning] cannot register global ")
    return
  dbgGlobalData.slots[i].name = name          # file.variableName
  dbgGlobalData.slots[i].typ = typ
  dbgGlobalData.slots[i].typStr = typStr
  dbgGlobalData.slots[i].address = address
  dbgGlobalData.slots[i].lineNr = line
  inc(dbgGlobalData.len)

proc dbgRegisterVariable(line: int, fname, typName, varName: cstring,
            address: pointer, typ: PNimType, typStr: cstring) {.compilerproc.} =
  # dbgRegisterVariable gets called when a variable is defined as the
  # debugging proceeds, and line holds the line Nr at which the variable is defined.
  #
  # Note: lineNr may change for a variable name if same variable name is
  #       used in different procs
  # Only registers variables for the current nim file
  #
  # line:      32
  # fname:     mycode.nim
  # typName:   "NTI_yzVtHjbKd39bygEUDLqI18Q_"   (maps to EOFError)
  # varName:   mycode.myError
  # address:   (void*) &.....
  # typ:       &NTI_......
  # typStr:    "EOFError"
  #
  if fname == framePtr.fileName:   # skip if not this file
    for i in 0.. <dbgVarData.vlen:
      if dbgVarData.vars[i].varName.data == varName:
        dbgVarData.vars[i].typeName.assign(typName)  # a diff proc but same var name
        dbgVarData.vars[i].lineNr = line             # a diff proc but same var name
        dbgVarData.vars[i].address = address
        dbgVarData.vars[i].typ = typ
        dbgVarData.vars[i].typeNameStr.assign(typStr)
        return
    # new entry
    let j = dbgVarData.vlen
    dbgVarData.vars[j].typeName.assign(typName)   # "NTI_....."
    dbgVarData.vars[j].varName.assign(varName)    # file.variableName
    dbgVarData.vars[j].lineNr = line              # nn (a number)
    dbgVarData.vars[j].address = address
    dbgVarData.vars[j].typ = typ                  # NTI_......
    dbgVarData.vars[j].typeNameStr.assign(typStr) # "int", "string", ...
    inc(dbgVarData.vlen)

proc endb(line: int, file: cstring, code: cstring) {.compilerproc, noinline.} =
  # This proc is called before every Nim code line!
  #
  # TFrame is defined in lib\nimbase.h as (subject to change)
  #  struct TFrame {
  #    TFrame* prev;
  #    NCSTRING procname;
  #    NI line;
  #    NCSTRING filename;
  #    NI16 len;
  #    NI16 calldepth;
  #  };
  if framePtr == nil: return
  framePtr.line = line       # this is done here for smaller code size!
  framePtr.filename = file
  if dbgLineHook != nil: dbgLineHook(code)

#------------------- retrieval of slot data -----------------------------

proc getLocal*(frame: PFrame; slot: int): VarSlot {.inline.} =
  ## retrieves the meta data for the local variable at `slot`. CAUTION: An
  ## invalid `slot` value causes a corruption!
  result = cast[PExtendedFrame](frame).slots[slot]

proc getLocalVar*(fp: PFrame, varname: cstring, slot: var VarSlot): bool =
  ## sets slot to the local variable if matched, and returns true,
  ## else return false
  ## where varname has NO filename prefixed
  for i in 0 .. fp.len-1:
    slot = getLocal(fp, i)
    if slot.name == varname: return true
  result = false

proc getLocalVarIndex*(fp: PFrame, varname: cstring): int =
  ## retrieves the index of the local variable if matched.
  ## else return -1
  ## where varname has NO filename prefixed
  var
    ss: StaticStr
    v: VarSlot
  for i in 0 .. fp.len-1:
    v = getLocal(fp, i)
    if v.name == varname: return i
  result = -1

proc getGlobalLen(): int {.inline.} =
  ## gets the number of registered globals.
  result = dbgGlobalData.len

proc getGlobal(slot: int): VarSlot {.inline.} =
  ## retrieves the meta data for the global variable at `slot`. CAUTION: An
  ## invalid `slot` value causes a corruption!
  result = dbgGlobalData.slots[slot]

proc getGlobalVar(varname: cstring, slot: var VarSlot): bool =
  ## retrieves the global variable if matched.
  ## else return nil
  ## where varname IS filename prefixed
  for i in 0 .. dbgGlobalData.len-1:
    slot = getGlobal(i)
    if slot.name == varname: return true
  result = false

proc getGlobalVarIndex(varname: cstring): int =
  ## retrieves the index of the global variable if matched.
  ## else return -1
  ## where varname IS filename prefixed
  var v: VarSlot
  for i in 0 .. dbgGlobalData.len-1:
    v = getGlobal(i)
    if v.name == varname: return i
  result = -1

proc getVarDataLen(): int {.inline.} =
  ## gets the number of registered variables (and their type).
  result = dbgVarData.vlen

proc getVarData(tSlot: int): VarTypSlot {.inline.} =
  ## retrieves the meta data for the variable Types at `slot`. CAUTION: An
  ## invalid `slot` value causes a corruption!
  result = dbgVarData.vars[tSlot]

proc getVarData(varName: cstring): VarTypSlot =
  ## retrieves the ``VarTypSlot`` info for the ``varName`` variable
  ## where the variable name is like "file.varname"
  result.address = nil
  for i in 0 .. dbgVarData.vlen-1:
    if dbgVarData.vars[i].varName == varName:
      return dbgVarData.vars[i]

# ------------------- breakpoint support ------------------------------------

proc addBreakpoint*(filename: cstring, lo, hi: int): bool =
  let x = dbgBPLen
  if x >= high(dbgBP): return false
  inc(dbgBPLen)
  result = true
  dbgBP[x].filename = filename
  dbgBP[x].low = lo
  dbgBP[x].high = hi
  for line in lo..hi: dbgBPbloom = dbgBPbloom or line

iterator listBreakpoints*(): ptr Breakpoint =
  ## lists all breakpoints.
  for i in 0.. <dbgBPLen: yield addr(dbgBP[i])

proc isActive*(b: ptr Breakpoint): bool = b.low > 0

proc flip*(b: ptr Breakpoint) =
  ## enables or disables 'b' depending on its current state.
  b.low = -b.low; b.high = -b.high

proc checkBreakpoints*(filename: cstring, line: int): ptr Breakpoint =
  ## in which breakpoint (if any) we are.
  if (dbgBPbloom and line) != line: return nil
  for b in listBreakpoints():
    if line >= b.low and line <= b.high and filename == b.filename: return b

# ------------------- watchpoint support ------------------------------------

proc `!&`(h: Hash, val: int): Hash {.inline.} =
  result = h +% val
  result = result +% result shl 10
  result = result xor (result shr 6)

proc `!$`(h: Hash): Hash {.inline.} =
  result = h +% h shl 3
  result = result xor (result shr 11)
  result = result +% result shl 15

proc hash(data: pointer, size: int): Hash =
  var h: Hash = 0
  var p = cast[cstring](data)
  var i = 0
  var s = size
  while s > 0:
    h = h !& ord(p[i])
    inc(i)
    dec(s)
  result = !$h

proc hashGcHeader(data: pointer): Hash =
  const headerSize = sizeof(int)*2
  result = hash(cast[pointer](cast[int](data) -% headerSize), headerSize)

proc genericHashAux(dest: pointer, mt: PNimType, shallow: bool,
                    h: Hash): Hash
proc genericHashAux(dest: pointer, n: ptr TNimNode, shallow: bool,
                    h: Hash): Hash =
  var d = cast[ByteAddress](dest)
  case n.kind
  of nkSlot:
    result = genericHashAux(cast[pointer](d +% n.offset), n.typ, shallow, h)
  of nkList:
    result = h
    for i in 0..n.len-1:
      result = result !& genericHashAux(dest, n.sons[i], shallow, result)
  of nkCase:
    result = h !& hash(cast[pointer](d +% n.offset), n.typ.size)
    var m = selectBranch(dest, n)
    if m != nil: result = genericHashAux(dest, m, shallow, result)
  of nkNone: sysAssert(false, "genericHashAux")

proc genericHashAux(dest: pointer, mt: PNimType, shallow: bool,
                    h: Hash): Hash =
  sysAssert(mt != nil, "genericHashAux 2")
  case mt.kind
  of tyString, tyCString:
    var x = cast[PPointer](dest)[]
    result = h
    if x != nil:
      let s = cast[NimString](x)
      when defined(trackGcHeaders):
        result = result !& hashGcHeader(x)
      else:
        result = result !& hash(x, s.len)
  of tySequence:
    var x = cast[PPointer](dest)
    var dst = cast[ByteAddress](cast[PPointer](dest)[])
    result = h
    if dst != 0:
      when defined(trackGcHeaders):
        result = result !& hashGcHeader(cast[PPointer](dest)[])
      else:
        for i in 0..cast[PGenericSeq](dst).len-1:
          result = result !& genericHashAux(
            cast[pointer](dst +% i*% mt.base.size +% GenericSeqSize),
            mt.base, shallow, result)
  of tyObject, tyTuple:
    # we don't need to copy m_type field for tyObject, as they are equal anyway
    result = genericHashAux(dest, mt.node, shallow, h)
  of tyArray, tyArrayConstr:
    let d = cast[ByteAddress](dest)
    result = h
    for i in 0..(mt.size div mt.base.size)-1:
      result = result !& genericHashAux(cast[pointer](d +% i*% mt.base.size),
                                        mt.base, shallow, result)
  of tyRef, tyVar, tyPtr:
    when defined(trackGcHeaders):
      var s = cast[PPointer](dest)[]
      if s != nil:
        result = result !& hashGcHeader(s)
    else:
      if shallow:
        result = h !& hash(dest, mt.size)
      else:
        result = h
        var s = cast[PPointer](dest)[]
        if s != nil:
          result = result !& genericHashAux(s, mt.base, shallow, result)
  else:
    result = h !& hash(dest, mt.size) # hash raw bits

proc genericHash(dest: pointer, mt: PNimType): int =
  result = genericHashAux(dest, mt, false, 0)

proc dbgUnregisterWatchpoints*() =
  watchPointsLen = 0

include "system/endb"
