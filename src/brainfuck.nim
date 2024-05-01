
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
    VMState* = object
        program*: seq[VMOp]
        programPtr*: int
        memory*: seq[int]
        memoryPtr*: int
        output*: seq[char]

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

proc translateOp(ast: AST): seq[VMOp] =
    case ast.kind
        of ASTKind.ModMemPtr: result = @[VMOp(kind: VMOpKind.ModMemPtr, ptrChangeBy: ast.ptrChangeBy)]
        of ASTKind.ModMem: result = @[VMOp(kind: VMOpKind.ModMem, dataChangeBy: ast.dataChangeBy)]
        of ASTKind.SetMem: result = @[VMOp(kind: VMOpKind.SetMem, data: ast.data)]
        of ASTKind.SysCall: result = @[VMOp(kind: VMOpKind.SysCall, sysCall: ast.sysCall)]
        of ASTKind.Loop:
            var loop: seq[VMOp] = @[]
            for i in 0 ..< ast.loop.len:
                loop.add(translateOp(ast.loop[i]))
            let endJump = VMOp(kind: VMOpKind.JumpIfZero, zeroJumpLoc: (loop.len + 2))
            let startJump = VMOp(kind: VMOpKind.JumpIfNotZero, notZeroJumpLoc: (-loop.len))
            result = @[endJump] & loop & @[startJump]


proc translateAst*(ast: seq[AST]): seq[VMOp] =
    for i in 0 ..< ast.len:
        result.add(translateOp(ast[i]))

    # Go back through the vmops and update the jump locations to be offset by the index
    for i in 0 ..< result.len:
        case result[i].kind
            of VMOpKind.JumpIfNotZero:
                result[i].notZeroJumpLoc += i
            of VMOpKind.JumpIfZero:
                result[i].zeroJumpLoc += i
            else: discard

proc runOne*(vm: var VMState) =
    var op: VMOp
    if vm.programPtr >= vm.program.len:
        op = VMOp(kind: VMOpKind.SysCall, sysCall: SysCall.Stop)
    else:
        op = vm.program[vm.programPtr]

    inc(vm.programPtr)

    case op.kind
        of VMOpKind.ModMemPtr:
            vm.memoryPtr = (vm.memoryPtr + op.ptrChangeBy + vm.memory.len) mod vm.memory.len
        of VMOpKind.ModMem:
            vm.memory[vm.memoryPtr] = (vm.memory[vm.memoryPtr] + op.dataChangeBy + 256) mod 256
        of VMOpKind.SetMem:
            vm.memory[vm.memoryPtr] = (op.data + 256) mod 256
        of VMOpKind.SysCall:
            case op.sysCall
                of SysCall.PutChar:
                    vm.output.add(vm.memory[vm.memoryPtr].chr)
                of SysCall.GetChar:
                    let nextChar = stdin.readChar
                    vm.memory[vm.memoryPtr] = nextChar.ord
                of SysCall.Stop:
                    vm.programPtr = vm.program.len
        of VMOpKind.JumpIfNotZero:
            if vm.memory[vm.memoryPtr] != 0:
                vm.programPtr = op.notZeroJumpLoc
        of VMOpKind.JumpIfZero:
            if vm.memory[vm.memoryPtr] == 0:
                vm.programPtr = op.zeroJumpLoc


proc readOutput*(vm: var VMState): seq[char] =
    result = vm.output
    vm.output = @[]

proc run*(vm: var VMState) =
    while vm.programPtr < vm.program.len:
        for i in 0 .. vm.memory.len:
            runOne(vm)
        for c in readOutput(vm):
            write(stdout, c)

when isMainModule:
    let helloWorld = "[---]++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

    let ast = helloWorld.parseAst
    let optimisedAst = ast.optimiseAst
    let vmOps = optimisedAst.translateAst

    var vmState = VMState(program: vmOps, programPtr: 0, memory: newSeq[int](30000), memoryPtr: 0, output: @[])

    while vmState.programPtr < vmState.program.len:
        runOne(vmState)
    for c in vmState.output:
        write(stdout, c)