package koticc.assembly

import koticc.ast.AST
import koticc.ast.Type
import koticc.tacky.Tacky

fun tackyProgramToAssembly(tackyProgram: Tacky.Program): Assembly.Program =
    TackyAssemblyGenerator(tackyProgram.symbolTable.toBackendSymbolTable()).tackyProgramToAssembly(tackyProgram)

class TackyAssemblyGenerator(private val symbolTable: BackendSymbolTable) {
    fun tackyProgramToAssembly(tackyProgram: Tacky.Program): Assembly.Program =
        Assembly.Program(
            topLevel = tackyProgram.topLevel.map { tackyTopLevelToAssembly(it) },
        )

    private fun tackyTopLevelToAssembly(tackyTopLevel: Tacky.TopLevel): Assembly.TopLevel =
        when (tackyTopLevel) {
            is Tacky.TopLevel.FunctionDefinition -> Assembly.TopLevel.FunctionDefinition(
                value = tackyFunctionDefinitionToAssembly(tackyTopLevel.value),
            )

            is Tacky.TopLevel.StaticVariable -> Assembly.TopLevel.StaticVariable(
                value = tackyStaticVariableToAssembly(tackyTopLevel.value),
            )
        }

    private fun tackyFunctionDefinitionToAssembly(tackyFunctionDefinition: Tacky.FunctionDefinition): Assembly.FunctionDefinition {
        val instructions = copyParameters(tackyFunctionDefinition.parameters) + tackyFunctionDefinition.body
            .flatMap(::tackyInstructionToAssembly)
        val withPseudoIdentifiersReplaced = PseudoIdentifierReplacer(symbolTable).replace(instructions)
        val withFixedOperands = withPseudoIdentifiersReplaced.flatMap(::fixInstructionOperands)
        return Assembly.FunctionDefinition(
            name = tackyFunctionDefinition.name,
            global = tackyFunctionDefinition.global,
            body = withFixedOperands,
        )
    }

    private fun copyParameters(parameters: List<String>): List<Assembly.Instruction> =
        parameters
            .mapIndexed { index, parameter ->
                val src = when {
                    index < argumentRegisters.size -> Assembly.Operand.Register(argumentRegisters[index])
                    else -> {
                        val stackParameterIndex = index - argumentRegisters.size
                        // 8(%rbp) contains the address of the caller
                        // 16(%rbp) is the first stack argument (6th), 24(%rbp) is the second stack argument (7th), etc.
                        Assembly.Operand.Stack(16 + stackParameterIndex * 8)
                    }
                }
                Assembly.Instruction.Mov(
                    type = symbolTable.objectSymbol(parameter).type,
                    src = src,
                    dst = Assembly.Operand.PseudoIdentifier(parameter),
                )
            }

    private val argumentRegisters = listOf(
        Assembly.RegisterValue.Di,
        Assembly.RegisterValue.Si,
        Assembly.RegisterValue.Dx,
        Assembly.RegisterValue.Cx,
        Assembly.RegisterValue.R8,
        Assembly.RegisterValue.R9,
    )

    private fun tackyStaticVariableToAssembly(tackyStaticVariable: Tacky.StaticVariable): Assembly.StaticVariable =
        Assembly.StaticVariable(
            name = tackyStaticVariable.name,
            global = tackyStaticVariable.global,
            initialValue = tackyStaticVariable.initialValue,
            alignment = when (tackyStaticVariable.type) {
                is Type.Int -> 4
                is Type.Long -> 8
            },
        )

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
                        type = tackyInstruction.src.assemblyType(),
                        src = tackyValueToOperand(tackyInstruction.src),
                        dst = tackyValueToOperand(tackyInstruction.dst),
                    ),
                )

            is Tacky.Instruction.JumpIfZero ->
                listOf(
                    Assembly.Instruction.Cmp(
                        type = tackyInstruction.src.assemblyType(),
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
                        type = tackyInstruction.src.assemblyType(),
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
                        type = tackyInstruction.value.assemblyType(),
                        src = tackyValueToOperand(tackyInstruction.value),
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                    ),
                    Assembly.Instruction.Ret,
                )

            is Tacky.Instruction.Call -> functionCall(tackyInstruction)

            is Tacky.Instruction.SignExtend -> listOf(
                Assembly.Instruction.Movsx(
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = tackyValueToOperand(tackyInstruction.dst),
                ),
            )

            is Tacky.Instruction.Truncate -> listOf(
                Assembly.Instruction.Mov(
                    type = Assembly.Type.LongWord,
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = tackyValueToOperand(tackyInstruction.dst),
                ),
            )
        }

    private fun tackyValueToOperand(tackyValue: Tacky.Value): Assembly.Operand =
        when (tackyValue) {
            is Tacky.Value.Constant -> {
                when (tackyValue.value) {
                    is AST.IntConstant -> Assembly.Operand.Immediate(tackyValue.value.value)
                    is AST.LongConstant -> TODO()
                }
            }
            is Tacky.Value.Variable -> {
                val objectSymbol = symbolTable.objectSymbol(tackyValue.name)
                if (objectSymbol.static) {
                    Assembly.Operand.Data(tackyValue.name)
                } else {
                    Assembly.Operand.PseudoIdentifier(tackyValue.name)
                }
            }
        }

    private fun simpleUnaryInstructions(
        operator: Assembly.UnaryOperator,
        src: Tacky.Value,
        dst: Tacky.Value,
    ): List<Assembly.Instruction> {
        val dstAssembly = tackyValueToOperand(dst)
        return listOf(
            Assembly.Instruction.Mov(
                type = src.assemblyType(),
                src = tackyValueToOperand(src),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Unary(
                operator = operator,
                type = src.assemblyType(),
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
                type = src.assemblyType(),
                src = Assembly.Operand.Immediate(0),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Cmp(
                type = src.assemblyType(),
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
                type = left.assemblyType(),
                src = leftAssembly,
                dst = dstAssembly,
            ),
            Assembly.Instruction.Binary(
                operator = operator,
                type = left.assemblyType(),
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
                type = left.assemblyType(),
                src = tackyValueToOperand(left),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
            ),
            Assembly.Instruction.Cdq(type = left.assemblyType()),
            Assembly.Instruction.Idiv(
                type = right.assemblyType(),
                operand = tackyValueToOperand(right),
            ),
            Assembly.Instruction.Mov(
                type = dst.assemblyType(),
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
                type = left.assemblyType(),
                src = Assembly.Operand.Immediate(0),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Cmp(
                type = left.assemblyType(),
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
                type = left.assemblyType(),
                src = tackyValueToOperand(left),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Mov(
                type = right.assemblyType(),
                src = tackyValueToOperand(right),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Cx),
            ),
            Assembly.Instruction.Shift(
                type = left.assemblyType(),
                operator = operator,
                dst = dstAssembly,
            ),
        )
    }

    private fun functionCall(call: Tacky.Instruction.Call): List<Assembly.Instruction> = buildList {
        val registerArguments = call.arguments.take(argumentRegisters.size)
        val stackArguments = call.arguments.drop(argumentRegisters.size)

        // stack must be 16-bytes aligned before issuing a call instruction
        // each stack argument is 8-bytes long, so if number is even, there's nothing to align
        val stackPadding = if (stackArguments.size % 2 == 0) {
            0
        } else {
            8
        }
        if (stackPadding != 0) {
            add(Assembly.Instruction.AllocateStack(stackPadding))
        }

        registerArguments.forEachIndexed { index, arg ->
            add(
                Assembly.Instruction.Mov(
                    type = arg.assemblyType(),
                    src = tackyValueToOperand(arg),
                    dst = Assembly.Operand.Register(argumentRegisters[index]),
                ),
            )
        }
        stackArguments.reversed().forEach { arg ->
            // even though our values can be 4-bytes, pushq accepts 8-bytes operands,
            // in case of immediate or register values there's no problem
            // but if the operand is the value somewhere in the memory, higher 4-bytes may lie in the inaccessible memory
            // to prevent that, we move the 4-bytes value to the register first and then push the register
            when (val assemblyArgument = tackyValueToOperand(arg)) {
                is Assembly.Operand.Immediate, is Assembly.Operand.Register -> {
                    add(Assembly.Instruction.Push(assemblyArgument))
                }

                else -> {
                    add(
                        Assembly.Instruction.Mov(
                            type = arg.assemblyType(),
                            src = assemblyArgument,
                            dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                        ),
                    )
                    add(Assembly.Instruction.Push(Assembly.Operand.Register(Assembly.RegisterValue.Ax)))
                }
            }
        }

        add(Assembly.Instruction.Call(call.name))

        val bytesToRemove = stackArguments.size * 8 + stackPadding
        if (bytesToRemove != 0) {
            add(Assembly.Instruction.DeallocateStack(bytesToRemove))
        }

        val assemblyDst = tackyValueToOperand(call.dst)
        add(
            Assembly.Instruction.Mov(
                type = call.dst.assemblyType(),
                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                dst = assemblyDst,
            ),
        )
    }

    private fun Tacky.Value.assemblyType() = when (this) {
        is Tacky.Value.Constant -> when (this.value) {
            is AST.IntConstant -> Assembly.Type.LongWord
            is AST.LongConstant -> TODO()
        }
        is Tacky.Value.Variable -> symbolTable.objectSymbol(name).type
    }
}

fun Assembly.Operand.isMemory() = when (this) {
    is Assembly.Operand.Data -> true
    is Assembly.Operand.Immediate -> false
    is Assembly.Operand.PseudoIdentifier -> true
    is Assembly.Operand.Register -> false
    is Assembly.Operand.Stack -> true
}
