discard """
  test that opening a nonexistent filename returns nil
  file: "tstreams2.nim"
  output: 'fs is: nil'
"""
import streams
var
  fs = newFileStream("nonexistentfilename.nim")
echo "fs is: ",repr(fs)
