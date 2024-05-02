import brainfuck
import streams

proc hello = compileFile("../examples/hello.b")
proc mandelbrot = compileFile("../examples/mandelbrot.b")

proc runInterpreted(filename: string) =
  var code: string
  if filename.len > 0:
    code = filename.readFile
  else:
    code = stdin.readAll

  let vmOps = code.parseAst.optimiseAst.translateAst
  var state = ProgramState(memory: newSeq[int](30000), memoryPtr: 0)
  var vm = VMState(program: vmOps, programPtr: 0, state: state)
  interpret(vm)

proc runCompiled(programName: string) =
  case programName:
    of "mandelbrot": mandelbrot()
    else: echo "Program not found"

when isMainModule:
  echo "Starting nim brainfuck interpreter..."

  import cligen
  dispatchMulti([runInterpreted], [runCompiled])