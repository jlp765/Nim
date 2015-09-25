discard """
  test opening non-existent file
  file: "tstreams2.nim"
  output: 'fs is: nil'
"""
import streams
var
  fs = newFileStream("filenamethatismissing.nim")
  line = ""
echo "fs is: ",repr(fs)
if not isNil(fs):
  while fs.readLine(line):
    echo line
  fs.close()
