import os
import brainfuck

when isMainModule:
  echo "Starting nim brainfuck interpreter..."

  let code = if paramCount() > 0: paramStr(1).readFile
             else: stdin.readAll

  let vmOps = code.parseAst.optimiseAst.translateAst
  var vmState: VMState = VMState(program: vmOps, programPtr: 0, memory: newSeq[int](30000), memoryPtr: 0, output: @[])

  run(vmState)

