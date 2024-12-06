package koticc

fun tackyProgramToAssembly(tackyProgram: Tacky.Program): Assembly.Program =
    Assembly.Program(
        functionDefinitions = tackyProgram.functionDefinitions.map(::tackyFunctionDefinitionToAssembly),
    )

private fun tackyFunctionDefinitionToAssembly(tackyFunctionDefinition: Tacky.FunctionDefinition): Assembly.FunctionDefinition {
    val body =
        tackyFunctionDefinition.body
            .flatMap(::tackyInstructionToAssembly)
            .let(::replacePseudoIdentifiers)
            .flatMap(::fixInstructionOperands)
    return Assembly.FunctionDefinition(
        name = tackyFunctionDefinition.name,
        body = body,
    )
}

private fun tackyInstructionToAssembly(tackyInstruction: Tacky.Instruction): List<Assembly.Instruction> =
    when (tackyInstruction) {
        is Tacky.Instruction.Unary -> {
            when (tackyInstruction.operator) {
                Tacky.UnaryOperator.Negate ->
                    simpleUnaryInstructions(
                        operator = Assembly.UnaryOperator.Neg,
                        src = tackyInstruction.src,
                        dst = tackyInstruction.dst,
                    )
                Tacky.UnaryOperator.Complement ->
                    simpleUnaryInstructions(
                        operator = Assembly.UnaryOperator.Not,
                        src = tackyInstruction.src,
                        dst = tackyInstruction.dst,
                    )
                Tacky.UnaryOperator.LogicalNegate -> logicalNotInstructions(tackyInstruction.src, tackyInstruction.dst)
            }
        }
        is Tacky.Instruction.Binary ->
            when (tackyInstruction.operator) {
                Tacky.BinaryOperator.Add ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.Add,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.Subtract ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.Sub,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.Multiply ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.Mul,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.BitwiseAnd ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.And,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.BitwiseOr ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.Or,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.BitwiseXor ->
                    simpleBinaryInstructions(
                        operator = Assembly.BinaryOperator.Xor,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.Divide ->
                    idivInstructions(
                        registerValue = Assembly.RegisterValue.Ax,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.Modulo ->
                    idivInstructions(
                        registerValue = Assembly.RegisterValue.Dx,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.LessThan ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.LessThan,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.LessThanOrEqual ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.LessThanOrEqual,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.GreaterThan ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.GreaterThan,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.GreaterThanOrEqual ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.GreaterThanOrEqual,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.Equal ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.Equal,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.NotEqual ->
                    cmpInstructions(
                        conditionalOperator = Assembly.ConditionalOperator.NotEqual,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.ShiftLeft ->
                    shiftInstructions(
                        operator = Assembly.ShiftOperator.Left,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
                Tacky.BinaryOperator.ShiftRight ->
                    shiftInstructions(
                        operator = Assembly.ShiftOperator.Right,
                        left = tackyInstruction.left,
                        right = tackyInstruction.right,
                        dst = tackyInstruction.dst,
                    )
            }
        is Tacky.Instruction.Copy ->
            listOf(
                Assembly.Instruction.Mov(
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = tackyValueToOperand(tackyInstruction.dst),
                ),
            )
        is Tacky.Instruction.JumpIfZero ->
            listOf(
                Assembly.Instruction.Cmp(
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = Assembly.Operand.Immediate(0),
                ),
                Assembly.Instruction.ConditionalJump(
                    operator = Assembly.ConditionalOperator.Equal,
                    target = tackyInstruction.target,
                ),
            )
        is Tacky.Instruction.JumpIfNotZero ->
            listOf(
                Assembly.Instruction.Cmp(
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = Assembly.Operand.Immediate(0),
                ),
                Assembly.Instruction.ConditionalJump(
                    operator = Assembly.ConditionalOperator.NotEqual,
                    target = tackyInstruction.target,
                ),
            )
        is Tacky.Instruction.Jump ->
            listOf(
                Assembly.Instruction.Jump(
                    target = tackyInstruction.target,
                ),
            )
        is Tacky.Instruction.Label ->
            listOf(
                Assembly.Instruction.Label(tackyInstruction.label),
            )
        is Tacky.Instruction.Return ->
            listOf(
                Assembly.Instruction.Mov(
                    src = tackyValueToOperand(tackyInstruction.value),
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                ),
                Assembly.Instruction.Ret,
            )
        is Tacky.Instruction.Call -> TODO()
    }

private fun tackyValueToOperand(tackyValue: Tacky.Value): Assembly.Operand =
    when (tackyValue) {
        is Tacky.Value.IntConstant -> Assembly.Operand.Immediate(tackyValue.value)
        is Tacky.Value.Variable -> Assembly.Operand.PseudoIdentifier(tackyValue.name)
    }

private fun simpleUnaryInstructions(
    operator: Assembly.UnaryOperator,
    src: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> {
    val dstAssembly = tackyValueToOperand(dst)
    return listOf(
        Assembly.Instruction.Mov(
            src = tackyValueToOperand(src),
            dst = dstAssembly,
        ),
        Assembly.Instruction.Unary(
            operator = operator,
            operand = dstAssembly,
        ),
    )
}

private fun logicalNotInstructions(
    src: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> {
    val dstAssembly = tackyValueToOperand(dst)
    return listOf(
        Assembly.Instruction.Mov(
            src = Assembly.Operand.Immediate(0),
            dst = dstAssembly,
        ),
        Assembly.Instruction.Cmp(
            src = tackyValueToOperand(src),
            dst = dstAssembly,
        ),
        Assembly.Instruction.Set(
            operator = Assembly.ConditionalOperator.Equal,
            dst = dstAssembly,
        ),
    )
}

private fun simpleBinaryInstructions(
    operator: Assembly.BinaryOperator,
    left: Tacky.Value,
    right: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> {
    val leftAssembly = tackyValueToOperand(left)
    val rightAssembly = tackyValueToOperand(right)
    val dstAssembly = tackyValueToOperand(dst)
    return listOf(
        Assembly.Instruction.Mov(
            src = leftAssembly,
            dst = dstAssembly,
        ),
        Assembly.Instruction.Binary(
            operator = operator,
            src = rightAssembly,
            dst = dstAssembly,
        ),
    )
}

private fun idivInstructions(
    registerValue: Assembly.RegisterValue,
    left: Tacky.Value,
    right: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> =
    listOf(
        Assembly.Instruction.Mov(
            src = tackyValueToOperand(left),
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
        ),
        Assembly.Instruction.Cdq,
        Assembly.Instruction.Idiv(
            operand = tackyValueToOperand(right),
        ),
        Assembly.Instruction.Mov(
            src = Assembly.Operand.Register(registerValue),
            dst = tackyValueToOperand(dst),
        ),
    )

private fun cmpInstructions(
    conditionalOperator: Assembly.ConditionalOperator,
    left: Tacky.Value,
    right: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> {
    val dstAssembly = tackyValueToOperand(dst)
    return listOf(
        Assembly.Instruction.Mov(
            src = Assembly.Operand.Immediate(0),
            dst = dstAssembly,
        ),
        Assembly.Instruction.Cmp(
            src = tackyValueToOperand(right),
            dst = tackyValueToOperand(left),
        ),
        Assembly.Instruction.Set(
            operator = conditionalOperator,
            dst = dstAssembly,
        ),
    )
}

private fun shiftInstructions(
    operator: Assembly.ShiftOperator,
    left: Tacky.Value,
    right: Tacky.Value,
    dst: Tacky.Value,
): List<Assembly.Instruction> {
    val dstAssembly = tackyValueToOperand(dst)
    return listOf(
        Assembly.Instruction.Mov(
            src = tackyValueToOperand(left),
            dst = dstAssembly,
        ),
        Assembly.Instruction.Mov(
            src = tackyValueToOperand(right),
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Cx),
        ),
        Assembly.Instruction.Shift(
            operator = operator,
            dst = dstAssembly,
        ),
    )
}

private fun replacePseudoIdentifiers(instructions: List<Assembly.Instruction>): List<Assembly.Instruction> {
    val stackOffsets = mutableMapOf<String, Int>()
    val result = ArrayList<Assembly.Instruction>(instructions.size + 1)
    result.add(Assembly.Instruction.AllocateStack(0))
    for (instruction in instructions) {
        val replacedInstruction =
            when (instruction) {
                is Assembly.Instruction.Mov -> {
                    val src = replacePseudoIdentifier(stackOffsets, instruction.src)
                    val dst = replacePseudoIdentifier(stackOffsets, instruction.dst)
                    Assembly.Instruction.Mov(src, dst)
                }

                is Assembly.Instruction.AllocateStack -> instruction
                is Assembly.Instruction.Binary -> {
                    val src = replacePseudoIdentifier(stackOffsets, instruction.src)
                    val dst = replacePseudoIdentifier(stackOffsets, instruction.dst)
                    Assembly.Instruction.Binary(instruction.operator, src, dst)
                }
                Assembly.Instruction.Cdq -> instruction
                is Assembly.Instruction.Cmp -> {
                    val src = replacePseudoIdentifier(stackOffsets, instruction.src)
                    val dst = replacePseudoIdentifier(stackOffsets, instruction.dst)
                    Assembly.Instruction.Cmp(src, dst)
                }
                is Assembly.Instruction.ConditionalJump -> instruction
                is Assembly.Instruction.Idiv -> {
                    val operand = replacePseudoIdentifier(stackOffsets, instruction.operand)
                    Assembly.Instruction.Idiv(operand)
                }
                is Assembly.Instruction.Jump -> instruction
                is Assembly.Instruction.Label -> instruction
                Assembly.Instruction.Ret -> instruction
                is Assembly.Instruction.Set -> {
                    val dst = replacePseudoIdentifier(stackOffsets, instruction.dst)
                    Assembly.Instruction.Set(instruction.operator, dst)
                }
                is Assembly.Instruction.Shift -> {
                    val dst = replacePseudoIdentifier(stackOffsets, instruction.dst)
                    Assembly.Instruction.Shift(instruction.operator, dst)
                }
                is Assembly.Instruction.Unary -> {
                    val operand = replacePseudoIdentifier(stackOffsets, instruction.operand)
                    Assembly.Instruction.Unary(instruction.operator, operand)
                }
            }
        result.add(replacedInstruction)
    }
    result[0] = Assembly.Instruction.AllocateStack(stackOffsets.size * 4)
    return result
}

private fun replacePseudoIdentifier(
    stackOffsets: MutableMap<String, Int>,
    operand: Assembly.Operand,
): Assembly.Operand =
    when (operand) {
        is Assembly.Operand.PseudoIdentifier -> {
            val stackOffset = stackOffsets.getOrPut(operand.name) { (stackOffsets.size + 1) * -4 }
            Assembly.Operand.Stack(stackOffset)
        }
        is Assembly.Operand.Register -> operand
        is Assembly.Operand.Immediate -> operand
        is Assembly.Operand.Stack -> operand
    }

private fun fixInstructionOperands(instruction: Assembly.Instruction): List<Assembly.Instruction> =
    when (instruction) {
        is Assembly.Instruction.AllocateStack -> listOf(instruction)
        is Assembly.Instruction.Binary -> {
            if (instruction.operator == Assembly.BinaryOperator.Mul && instruction.dst is Assembly.Operand.Stack) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.dst,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                    ),
                    Assembly.Instruction.Binary(
                        operator = instruction.operator,
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                    ),
                    Assembly.Instruction.Mov(
                        src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                        dst = instruction.dst,
                    ),
                )
            } else if (instruction.src is Assembly.Operand.Stack && instruction.dst is Assembly.Operand.Stack) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                    Assembly.Instruction.Binary(
                        operator = instruction.operator,
                        src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                        dst = instruction.dst,
                    ),
                )
            } else {
                listOf(instruction)
            }
        }
        Assembly.Instruction.Cdq -> listOf(instruction)
        is Assembly.Instruction.Cmp -> {
            if (instruction.src is Assembly.Operand.Stack && instruction.dst is Assembly.Operand.Stack) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                    Assembly.Instruction.Cmp(
                        src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                        dst = instruction.dst,
                    ),
                )
            } else if (instruction.dst is Assembly.Operand.Immediate) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.dst,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                    Assembly.Instruction.Cmp(
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                )
            } else {
                listOf(instruction)
            }
        }
        is Assembly.Instruction.ConditionalJump -> listOf(instruction)
        is Assembly.Instruction.Idiv -> {
            if (instruction.operand is Assembly.Operand.Immediate) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.operand,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                    Assembly.Instruction.Idiv(
                        operand = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                )
            } else {
                listOf(instruction)
            }
        }
        is Assembly.Instruction.Jump -> listOf(instruction)
        is Assembly.Instruction.Label -> listOf(instruction)
        is Assembly.Instruction.Mov -> {
            if (instruction.src is Assembly.Operand.Stack && instruction.dst is Assembly.Operand.Stack) {
                listOf(
                    Assembly.Instruction.Mov(
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                    Assembly.Instruction.Mov(
                        src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                        dst = instruction.dst,
                    ),
                )
            } else {
                listOf(instruction)
            }
        }
        Assembly.Instruction.Ret -> listOf(instruction)
        is Assembly.Instruction.Set -> listOf(instruction)
        is Assembly.Instruction.Shift -> listOf(instruction)
        is Assembly.Instruction.Unary -> listOf(instruction)
    }
