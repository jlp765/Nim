discard """
  test opening a missing file by newFileStream() returns nil
  file: "tstreams2.nim"
  output: 'fs is: nil'
"""
import streams
var
  fs = newFileStream("filethatismissing.txt")
  line = ""
echo "fs is: ",repr(fs)
if not isNil(fs):
  while fs.readLine(line):
    echo line
  fs.close()
