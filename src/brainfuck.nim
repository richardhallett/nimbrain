import macros
import streams

type
  SysCall {.pure.} = enum
    PutChar, GetChar, Stop

type
  ASTKind {.pure.} = enum
    ModMemPtr, ModMem, SetMem, SysCall, Loop
  AST = object
    case kind: ASTKind
      of ModMemPtr: ptrChangeBy: int
      of ModMem: dataChangeBy: int
      of SetMem: data: int
      of SysCall: sysCall: SysCall
      of Loop: loop: seq[AST]

# This is so we can compare ASTs for equality
proc `==`(a, b: AST): bool =
  result = a.kind == b.kind
  case a.kind
    of ASTKind.ModMemPtr: result = result and a.ptrChangeBy == b.ptrChangeBy
    of ASTKind.ModMem: result = result and a.dataChangeBy == b.dataChangeBy
    of ASTKind.SetMem: result = result and a.data == b.data
    of ASTKind.SysCall: result = result and a.sysCall == b.sysCall
    of ASTKind.Loop:
        result = result and a.loop.len == b.loop.len
        for i in 0 ..< a.loop.len:
            result = result and a.loop[i] == b.loop[i]

type
  VMOpKind* {.pure.} = enum
    ModMemPtr, ModMem, SetMem, SysCall, JumpIfNotZero, JumpIfZero
  VMOp* = object
    case kind*: VMOpKind
      of ModMemPtr: ptrChangeBy*: int
      of ModMem: dataChangeBy*: int
      of SetMem: data*: int
      of SysCall: sysCall*: SysCall
      of JumpIfNotZero: notZeroJumpLoc*: int
      of JumpIfZero: zeroJumpLoc*: int

type
  ProgramState* = object
    memory*: seq[int]
    memoryPtr*: int

type
  VMState* = object
    program*: seq[VMOp]
    programPtr*: int
    state*: ProgramState

proc parseAst*(source: string): seq[AST] =
  var currentBlock = result
  var blockStack: seq[seq[AST]] = @[]
  for c in source:
    case c
      of '+': currentBlock.add(AST(kind: ASTKind.ModMem, dataChangeBy: 1))
      of '-': currentBlock.add(AST(kind: ASTKind.ModMem, dataChangeBy: -1))
      of '>': currentBlock.add(AST(kind: ASTKind.ModMemPtr, ptrChangeBy: 1))
      of '<': currentBlock.add(AST(kind: ASTKind.ModMemPtr, ptrChangeBy: -1))
      of '.': currentBlock.add(AST(kind: ASTKind.SysCall, sysCall: SysCall.PutChar))
      of ',': currentBlock.add(AST(kind: ASTKind.SysCall, sysCall: SysCall.GetChar))
      of '[':
        blockStack.add(currentBlock)
        currentBlock = @[]
      of ']':
        var loop = currentBlock
        currentBlock = blockStack.pop
        currentBlock.add(AST(kind: ASTKind.Loop, loop: loop))
      else: discard

  return currentBlock

proc optimiseAst*(ast: seq[AST]): seq[AST] =
    # Collapase sequential instructions into combined operations e.g. ++++ modifies memory by 4
    proc collapseSeq(ast: seq[AST]): seq[AST] =
      for e in ast:
        if result.len > 0:
          let l = result[^1]
          if e.kind == ASTKind.ModMem and l.kind == ASTKind.ModMem:
            result[^1] = AST(kind: ASTKind.ModMem, dataChangeBy: e.dataChangeBy + l.dataChangeBy)
          elif e.kind == ASTKind.ModMemPtr and l.kind == ASTKind.ModMemPtr:
            result[^1] = AST(kind: ASTKind.ModMemPtr, ptrChangeBy: e.ptrChangeBy + l.ptrChangeBy)
          elif e.kind == ASTKind.Loop:
            result.add(AST(kind: ASTKind.Loop, loop: collapseSeq(e.loop)))
          else:
            result.add(e)
        else:
          result.add(e)
      return result

    # Remove any loops that are just setting memory to 0 i.e. [-] or [+] etc.
    # It works for [+] because memory wraps at 255 eventually so it will reach 0
    proc clearLoop(ast: seq[AST]): seq[AST] =
      for e in ast:
        case e.kind
          of ASTKind.Loop:
            if e.loop.len == 1 and e.loop[0].kind == ASTKind.ModMem:
              result.add(AST(kind: ASTKind.SetMem, data: 0))
            else:
              result.add(AST(kind: ASTKind.Loop, loop: clearLoop(e.loop)))
          else:
              result.add(e)
      return result

    var optimisedAst = ast.collapseSeq.clearLoop

    return optimisedAst

proc translateToVMOp(ast: AST): seq[VMOp] =
  case ast.kind
    of ASTKind.ModMemPtr: result = @[VMOp(kind: VMOpKind.ModMemPtr, ptrChangeBy: ast.ptrChangeBy)]
    of ASTKind.ModMem: result = @[VMOp(kind: VMOpKind.ModMem, dataChangeBy: ast.dataChangeBy)]
    of ASTKind.SetMem: result = @[VMOp(kind: VMOpKind.SetMem, data: ast.data)]
    of ASTKind.SysCall: result = @[VMOp(kind: VMOpKind.SysCall, sysCall: ast.sysCall)]
    of ASTKind.Loop:
      var loop: seq[VMOp] = @[]
      for i in 0 ..< ast.loop.len:
        loop.add(translateToVMOp(ast.loop[i]))
      let endJump = VMOp(kind: VMOpKind.JumpIfZero, zeroJumpLoc: (loop.len + 2))
      let startJump = VMOp(kind: VMOpKind.JumpIfNotZero, notZeroJumpLoc: (-loop.len))
      result = @[endJump] & loop & @[startJump]


proc translateAst*(ast: seq[AST]): seq[VMOp] =
    for i in 0 ..< ast.len:
      result.add(translateToVMOp(ast[i]))

    # Go back through the vmops and update the jump locations to be offset by the index
    for i in 0 ..< result.len:
      case result[i].kind
        of VMOpKind.JumpIfNotZero:
          result[i].notZeroJumpLoc += i
        of VMOpKind.JumpIfZero:
          result[i].zeroJumpLoc += i
        else: discard

proc compile(ast: seq[AST], input, output: string): NimNode =

  var stmts = @[newStmtList()]

  template addStmt(text): void =
    stmts[stmts.high].add parseStmt(text)

  addStmt "var state = ProgramState(memory: newSeq[int](30000), memoryPtr: 0)"

  addStmt "var inputStream = " & input
  addStmt "var outputStream = " & output

  proc buildStatements(ast: seq[AST]): NimNode =
    var stmts = @[newStmtList()]

    template addStmt(text): void =
      stmts[stmts.high].add parseStmt(text)

    for astNode in ast:
      case astNode.kind
        of ASTKind.ModMemPtr:
          addStmt "state.memoryPtr = state.memoryPtr + " & $astNode.ptrChangeBy
        of ASTKind.ModMem:
          addStmt "state.memory[state.memoryPtr] = state.memory[state.memoryPtr] + " & $astNode.dataChangeBy
        of ASTKind.SetMem:
          addStmt "state.memory[state.memoryPtr] = " & $astNode.data
        of ASTKind.SysCall:
          case astNode.sysCall
            of SysCall.PutChar:
              addStmt "outputStream.write(state.memory[state.memoryPtr].chr)"
            of SysCall.GetChar:
              addStmt "let nextChar = inputStream.readChar"
              addStmt "state.memory[state.memoryPtr] = nextChar.ord"
            else: discard
        of ASTKind.Loop:
          let loopStmts = buildStatements(astNode.loop)
          var whileLoop = newNimNode(nnkWhileStmt)
          whileLoop.add parseExpr "state.memory[state.memoryPtr] != 0"
          whileLoop.add loopStmts
          stmts[stmts.high].add whileLoop
    result = stmts[0]

  stmts[0].add buildStatements(ast)

  result = stmts[0]

macro compileString*(code: string) =
  compile(code.strVal.parseAst.optimiseAst, "stdin.newFileStream", "stdout.newFileStream")

macro compileFile*(filename: string) =
  var code = staticRead(filename.strVal)
  compile(code.parseAst.optimiseAst, "stdin.newFileStream", "stdout.newFileStream")

proc interpretOne*(vm: var VMState, input, output: Stream) =
  var op: VMOp
  if vm.programPtr >= vm.program.len:
    op = VMOp(kind: VMOpKind.SysCall, sysCall: SysCall.Stop)
  else:
    op = vm.program[vm.programPtr]

  inc(vm.programPtr)

  case op.kind
    of VMOpKind.ModMemPtr:
      vm.state.memoryPtr = (vm.state.memoryPtr + op.ptrChangeBy + vm.state.memory.len) mod vm.state.memory.len
    of VMOpKind.ModMem:
      vm.state.memory[vm.state.memoryPtr] = (vm.state.memory[vm.state.memoryPtr] + op.dataChangeBy + 256) mod 256
    of VMOpKind.SetMem:
      vm.state.memory[vm.state.memoryPtr] = (op.data + 256) mod 256
    of VMOpKind.SysCall:
      case op.sysCall
        of SysCall.PutChar:
          output.write(vm.state.memory[vm.state.memoryPtr].chr)
        of SysCall.GetChar:
          let nextChar = input.readChar
          vm.state.memory[vm.state.memoryPtr] = nextChar.ord
        of SysCall.Stop:
          vm.programPtr = vm.program.len
    of VMOpKind.JumpIfNotZero:
      if vm.state.memory[vm.state.memoryPtr] != 0:
        vm.programPtr = op.notZeroJumpLoc
    of VMOpKind.JumpIfZero:
      if vm.state.memory[vm.state.memoryPtr] == 0:
        vm.programPtr = op.zeroJumpLoc


proc interpret*(vm: var VMState, input, output: Stream) =
  while vm.programPtr < vm.program.len:
    for i in 0 .. vm.state.memory.len:
      interpretOne(vm, input, output)

proc interpret*(vm: var VMState) =
  interpret(vm, stdin.newFileStream, stdout.newFileStream)

when isMainModule:
  # Just some testing code
  proc helloWorld = compileString("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  helloWorld()

  let ast = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.".parseAst
  let optimisedAst = ast.optimiseAst
  let vmOps = optimisedAst.translateAst
  var state = ProgramState(memory: newSeq[int](30000), memoryPtr: 0)
  var vm = VMState(program: vmOps, programPtr: 0, state: state)
  interpret(vm)