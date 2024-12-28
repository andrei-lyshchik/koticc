package koticc.assembly

import koticc.ast.AST
import koticc.semantic.SymbolTable
import koticc.semantic.VariableAttributes
import koticc.semantic.variableSymbolOrNull
import koticc.tacky.Tacky

fun tackyProgramToAssembly(tackyProgram: Tacky.Program, symbolTable: SymbolTable): Assembly.Program =
    TackyAssemblyGenerator(symbolTable).tackyProgramToAssembly(tackyProgram)

class TackyAssemblyGenerator(private val symbolTable: SymbolTable) {
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
        val body =
            (
                copyParameters(tackyFunctionDefinition.parameters) + tackyFunctionDefinition.body
                    .flatMap(::tackyInstructionToAssembly)
                )
                .let(::replacePseudoIdentifiers)
                .flatMap(::fixInstructionOperands)
        return Assembly.FunctionDefinition(
            name = tackyFunctionDefinition.name,
            global = tackyFunctionDefinition.global,
            body = body,
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

            is Tacky.Instruction.Call -> functionCall(tackyInstruction)
        }

    private fun tackyValueToOperand(tackyValue: Tacky.Value): Assembly.Operand =
        when (tackyValue) {
            is Tacky.Value.IntConstant -> {
                when (tackyValue.value) {
                    is AST.IntConstant -> Assembly.Operand.Immediate(tackyValue.value.value)
                }
            }
            is Tacky.Value.Variable -> {
                val variableSymbol = symbolTable.variableSymbolOrNull(tackyValue.name)
                if (variableSymbol == null) {
                    // if the variable is not found in the symbol table, it's a temp tacky variable, so it's a local/lives on the stack
                    Assembly.Operand.PseudoIdentifier(tackyValue.name)
                } else {
                    when (variableSymbol.attributes) {
                        VariableAttributes.Local -> Assembly.Operand.PseudoIdentifier(tackyValue.name)
                        is VariableAttributes.Static -> Assembly.Operand.Data(tackyValue.name)
                    }
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
                    src = tackyValueToOperand(arg),
                    dst = Assembly.Operand.Register(argumentRegisters[index]),
                ),
            )
        }
        stackArguments.reversed().forEach { arg ->
            // even though our values are 4-bytes, pushq accepts 8-bytes operands,
            // in case of immediate or register values there's no problem
            // but if the operand is the value somewhere in the memory, higher 4-bytes may lie in the inaccessible memory
            // to prevent that, we move the 4-bytes value to the register first and then push the register
            when (val assemblyArgument = tackyValueToOperand(arg)) {
                is Assembly.Operand.Immediate, is Assembly.Operand.Register -> {
                    add(Assembly.Instruction.Push(assemblyArgument))
                }

                else -> {
                    add(Assembly.Instruction.Mov(assemblyArgument, Assembly.Operand.Register(Assembly.RegisterValue.Ax)))
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
        add(Assembly.Instruction.Mov(Assembly.Operand.Register(Assembly.RegisterValue.Ax), assemblyDst))
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

                    is Assembly.Instruction.Call -> instruction
                    is Assembly.Instruction.DeallocateStack -> instruction
                    is Assembly.Instruction.Push -> {
                        val operand = replacePseudoIdentifier(stackOffsets, instruction.operand)
                        Assembly.Instruction.Push(operand)
                    }
                }
            result.add(replacedInstruction)
        }
        val stackBytes = stackOffsets.size * 4
        val alignedStackBytes = if (stackBytes % 16 == 0) {
            stackBytes
        } else {
            val padding = 16 - stackBytes % 16
            stackBytes + padding
        }
        result[0] = Assembly.Instruction.AllocateStack(alignedStackBytes)
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
            is Assembly.Operand.Data -> operand
        }

    private fun fixInstructionOperands(instruction: Assembly.Instruction): List<Assembly.Instruction> =
        when (instruction) {
            is Assembly.Instruction.AllocateStack -> listOf(instruction)
            is Assembly.Instruction.Binary -> {
                if (instruction.operator == Assembly.BinaryOperator.Mul && instruction.dst.isMemory()) {
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
                } else if (instruction.src.isMemory() && instruction.dst.isMemory()) {
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
                if (instruction.src.isMemory() && instruction.dst.isMemory()) {
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
                if (instruction.src.isMemory() && instruction.dst.isMemory()) {
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
            is Assembly.Instruction.Call -> listOf(instruction)
            is Assembly.Instruction.DeallocateStack -> listOf(instruction)
            is Assembly.Instruction.Push -> listOf(instruction)
        }
}

fun Assembly.Operand.isMemory() = when (this) {
    is Assembly.Operand.Data -> true
    is Assembly.Operand.Immediate -> false
    is Assembly.Operand.PseudoIdentifier -> true
    is Assembly.Operand.Register -> false
    is Assembly.Operand.Stack -> true
}
