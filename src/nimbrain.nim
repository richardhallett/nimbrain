import os

when isMainModule:
  echo "Welcome to brainfuck"
  let code = if paramCount() > 0: paramStr(1).readFile
             else: stdin.readAll
  echo "test"
  echo code
