discard """
  line: 6
  errormsg: "iterator within for loop context expected"
"""
# implicit items/pairs, but not if we have 3 for loop vars:
for x, y, z in {'a'..'z'}: #ERROR_MSG iterator within for loop context expected
  nil
