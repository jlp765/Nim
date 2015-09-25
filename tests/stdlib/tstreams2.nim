discard """
  test newFileStream() opening missing file
  file: "tstreams2.nim"
  output: 'fs is: nil'
"""
import streams
var
  fs = newFileStream("test_streams_ex1.nim")
  line = ""
echo "fs is: ",repr(fs)
if not isNil(fs):
  while fs.readLine(line):
    echo line
  fs.close()
