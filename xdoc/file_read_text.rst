Reading Text from a File
========================

* there are 9 different variations showing reading a text file
* this is not an exhaustive list of how to read a text file
* each reading of the input file is timed
* each file reading is approximately ordered from slower to faster
* if the examples were to manipulate strings within the loop, then the overall times would be much slower
* Garbage collection and OS load will add variability to the timing (this is NOT a benchmark)
* Provide some suitably sized (text) file called junkdata.txt when trying this example
* compile with -d:release (the file reading is slower otherwise)
  

.. code-block:: nim

  import strutils, times, os, parsecsv, streams, memfiles

  const
    fgetSz = 2500
    fname = "junkdata.txt"

  var
    t0, t1 = cpuTime()
    f: File
    line = ""
    cntL = 0
    cntC = 0
    fgetStr: cstring = newString(fgetSz+1)

  # WARNING: shouldn't really call this c library function, but use the Nim system procs,
  # because the Nim procs do extra code saftey checks
  #
  proc fgets(c: cstring, n: cint, f: File): cstring  {.importc: "fgets", header: "<stdio.h>".}

  iterator fgetsLine(f: File): string =
    while not isNil(fgets(fgetStr, fgetSz, f)):
      yield $fgetStr

  proc fgetsAll(f: File): string =
    result = ""
    for x in f.fgetsLine():
      result.add(x)

  template prepare(): untyped =
    t0 = cpuTime()
    cntL = 0
    cntC = 0

  template finalise(msg: static[string]): untyped =
    t1 = cpuTime()
    let s = (msg & repeat(' ',25))[0..25]
    echo s,($(t1-t0) & "00")[0..4], "  Lines: ", cntL, " chars: ",cntC

  # -- MAIN ---
  proc main() =
    # Check file exists
    if not open(f, fname):
      quit("Unable to find " & fname)
    close(f)

    # using fgets() from stdlib of C  (reading all file data into a single string)
    #
    # This is slower than the next fgets() example because of string handling
    #
    prepare()
    if open(f, fname):
      let s = f.fgetsAll
      cntL = s.countLines
      cntC = s.len - cntL   # exclude line termination
      close(f)
    finalise("fgets all: ")

    # using fgets() from stdlib of C
    #
    # This shows how to make C library calls
    # Safety-wise, it is best to use Nim's system lib procs
    #
    prepare()
    if open(f, fname):
      for s in f.fgetsLine:
        inc cntL
        cntC += s.len - 1   # exclude nl char
      close(f)
    finalise("fgets iterator: ")

    # Reading line-by-line using the readLine() iterator from the Nim system lib
    #
    # Compare this readLine() with the iterator lines()
    #
    prepare()
    if open(f, fname):
      while f.readLine(line):
        inc cntL
        cntC += line.len
      close(f)
    finalise("readLine: ")

    # using the lines() iterator from the Nim system lib
    #
    prepare()
    if open(f, fname):
      for line in f.lines:
        inc cntL
        cntC += line.len
      close(f)
    finalise("lines: ")

    # using the Nim parseCsv lib
    #
    # If you needed to manipulate string portions of each line, 
    # then this may be a quicker approach, 
    # because it provides the line already split 
    # by delimeter (if required)
    #
    prepare()
    var strm = newfileStream(fname, fmRead)
    if strm != nil:
      var cp: CsvParser
      open(cp, strm, fname)
      while cp.readRow():
        inc cntL
        for z in cp.row:
          cntC += z.len
      close(cp)
    finalise("parsecsv: ")

    # using readAll() and splitLines() from Nim system lib
    #
    # This would not be appropriate for large files due to
    # memory usage in reading ALL the file into a string
    # but for small files works well
    #
    prepare()
    if open(f, fname):
      let x = f.readAll
      for line in x.splitLines:
        inc cntL
        cntC += line.len
      close(f)
      dec cntL
    finalise("readAll splitLines: ")

    # using readAll() and splitLines() from Nim system lib
    #
    # This is a minor improvement on string handling compared
    # to the previous example (if you don't need access to the 
    # full text from the file)
    #
    prepare()
    if open(f, fname):
      for line in f.readAll().splitLines:
        inc cntL
        cntC += line.len
      close(f)
      dec cntL
    finalise("readAll().splitLines: ")

    # using readAll() and countLines() from Nim system and strutils lib
    #
    # This avoids manipulating the file on a per-line basis
    # and is probably not a practical example for most cases,
    # but does highlight that you may make performance gains if
    # you optimise your code for a specific situation
    #
    prepare()
    if open(f, fname):
      let x = f.readAll
      cntL = x.countLines
      cntC = x.len() - cntL   # exclude line termination
      close(f)
    finalise("readAll countlines: ")

    # using memfiles() and lines() from the Nim memfiles lib
    #
    # This is faster because the file is read into a buffer in chunks
    #
    prepare()
    var file = memfiles.open(fname, fmRead)
    for line in memfiles.lines(file):
      inc cntL
      cntC += line.len
    close(file)
    finalise("memfiles lines: ")

  main()
