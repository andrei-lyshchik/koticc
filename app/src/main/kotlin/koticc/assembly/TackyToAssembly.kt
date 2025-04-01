package koticc.assembly

import koticc.ast.AST
import koticc.ast.LabelName
import koticc.ast.Type
import koticc.semantic.InitialConstantValue
import koticc.tacky.Tacky

fun tackyProgramToAssembly(tackyProgram: Tacky.Program): Assembly.Program =
    TackyAssemblyGenerator(tackyProgram.symbolTable.toBackendSymbolTable()).tackyProgramToAssembly(tackyProgram)

class TackyAssemblyGenerator(private val symbolTable: BackendSymbolTable) {
    private data class ConstantKey(val value: InitialConstantValue, val alignment: Int)
    private val constantMap = mutableMapOf<ConstantKey, Assembly.StaticConstant>()

    private var labelCount = 0

    // there's no immediate operand for double values, so we have to store them in the data section
    private fun doubleConstantToOperand(value: Double, alignment: Int): Assembly.Operand.Data {
        val doubleString = value.toString()
            .replace('.', '_')
            .replace('-', '_')
        val name = "static_double_$doubleString"
        val staticConstant = constantMap.getOrPut(ConstantKey(InitialConstantValue.Double(value), alignment)) {
            Assembly.StaticConstant(
                name = name,
                alignment = alignment,
                value = InitialConstantValue.Double(value),
            )
        }
        return Assembly.Operand.Data(staticConstant.name)
    }

    private fun nextLabel(prefix: String) = LabelName("$prefix.${labelCount++}")

    fun tackyProgramToAssembly(tackyProgram: Tacky.Program): Assembly.Program =
        Assembly.Program(
            topLevel = tackyProgram.topLevel.map { tackyTopLevelToAssembly(it) } + constantMap.values.map { Assembly.TopLevel.StaticConstant(it) },
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

    private fun copyParameters(parameters: List<String>): List<Assembly.Instruction> {
        val classifiedParameters = classifyParameters(parameters) { parameter ->
            symbolTable.objectSymbol(parameter).type
        }
        val moveIntRegisterArguments = classifiedParameters.intRegisterArguments.mapIndexed { index, parameter ->
            Assembly.Instruction.Mov(
                type = symbolTable.objectSymbol(parameter).type,
                src = Assembly.Operand.Register(intArgumentRegisters[index]),
                dst = Assembly.Operand.PseudoIdentifier(parameter),
            )
        }
        val moveDoubleRegisterArguments = classifiedParameters.doubleRegisterArguments.mapIndexed { index, parameter ->
            Assembly.Instruction.Mov(
                type = symbolTable.objectSymbol(parameter).type,
                src = Assembly.Operand.Register(doubleArgumentRegisters[index]),
                dst = Assembly.Operand.PseudoIdentifier(parameter),
            )
        }
        val moveStackArguments = classifiedParameters.stackArguments.mapIndexed { index, parameter ->
            Assembly.Instruction.Mov(
                type = symbolTable.objectSymbol(parameter).type,
                // 8(%rbp) contains the address of the caller
                // 16(%rbp) is the first stack argument, 24(%rbp) is the second stack argument, etc.
                src = Assembly.Operand.Stack(16 + index * 8),
                dst = Assembly.Operand.PseudoIdentifier(parameter),
            )
        }
        return moveIntRegisterArguments + moveDoubleRegisterArguments + moveStackArguments
    }

    data class ClassifiedParameters<T>(
        val intRegisterArguments: List<T>,
        val doubleRegisterArguments: List<T>,
        val stackArguments: List<T>,
    )

    private fun <T> classifyParameters(parameters: List<T>, assemblyType: (T) -> Assembly.Type): ClassifiedParameters<T> {
        val intRegisterArguments = mutableListOf<T>()
        val doubleRegisterArguments = mutableListOf<T>()
        val stackArguments = mutableListOf<T>()
        parameters.forEach { parameter ->
            when (assemblyType(parameter)) {
                Assembly.Type.Double -> {
                    if (doubleRegisterArguments.size < doubleArgumentRegisters.size) {
                        doubleRegisterArguments.add(parameter)
                    } else {
                        stackArguments.add(parameter)
                    }
                }
                else -> {
                    if (intRegisterArguments.size < intArgumentRegisters.size) {
                        intRegisterArguments.add(parameter)
                    } else {
                        stackArguments.add(parameter)
                    }
                }
            }
        }
        return ClassifiedParameters(intRegisterArguments, doubleRegisterArguments, stackArguments)
    }

    private val intArgumentRegisters = listOf(
        Assembly.RegisterValue.Di,
        Assembly.RegisterValue.Si,
        Assembly.RegisterValue.Dx,
        Assembly.RegisterValue.Cx,
        Assembly.RegisterValue.R8,
        Assembly.RegisterValue.R9,
    )

    private val doubleArgumentRegisters = listOf(
        Assembly.RegisterValue.Xmm0,
        Assembly.RegisterValue.Xmm1,
        Assembly.RegisterValue.Xmm2,
        Assembly.RegisterValue.Xmm3,
        Assembly.RegisterValue.Xmm4,
        Assembly.RegisterValue.Xmm5,
        Assembly.RegisterValue.Xmm6,
        Assembly.RegisterValue.Xmm7,
    )

    private fun tackyStaticVariableToAssembly(tackyStaticVariable: Tacky.StaticVariable): Assembly.StaticVariable =
        Assembly.StaticVariable(
            name = tackyStaticVariable.name,
            global = tackyStaticVariable.global,
            initialValue = tackyStaticVariable.initialValue,
            alignment = when (tackyStaticVariable.type) {
                is Type.Int -> 4
                is Type.UInt -> 4
                is Type.Long -> 8
                is Type.ULong -> 8
                is Type.Double -> 8
            },
        )

    private fun tackyInstructionToAssembly(tackyInstruction: Tacky.Instruction): List<Assembly.Instruction> =
        when (tackyInstruction) {
            is Tacky.Instruction.Unary -> {
                when (tackyInstruction.operator) {
                    Tacky.UnaryOperator.Negate ->
                        if (tackyInstruction.src.assemblyType() == Assembly.Type.Double) {
                            doubleUnaryNegateInstructions(
                                src = tackyInstruction.src,
                                dst = tackyInstruction.dst,
                            )
                        } else {
                            simpleUnaryInstructions(
                                operator = Assembly.UnaryOperator.Neg,
                                src = tackyInstruction.src,
                                dst = tackyInstruction.dst,
                            )
                        }

                    Tacky.UnaryOperator.Complement ->
                        simpleUnaryInstructions(
                            operator = Assembly.UnaryOperator.Not,
                            src = tackyInstruction.src,
                            dst = tackyInstruction.dst,
                        )

                    Tacky.UnaryOperator.LogicalNegate -> {
                        if (tackyInstruction.src.assemblyType() == Assembly.Type.Double) {
                            doubleLogicalNotInstructions(tackyInstruction.src, tackyInstruction.dst)
                        } else {
                            logicalNotInstructions(tackyInstruction.src, tackyInstruction.dst)
                        }
                    }
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

                    Tacky.BinaryOperator.Divide -> {
                        if (tackyInstruction.left.assemblyType() == Assembly.Type.Double) {
                            simpleBinaryInstructions(
                                operator = Assembly.BinaryOperator.DivDouble,
                                left = tackyInstruction.left,
                                right = tackyInstruction.right,
                                dst = tackyInstruction.dst,
                            )
                        } else {
                            idivInstructions(
                                registerValue = Assembly.RegisterValue.Ax,
                                left = tackyInstruction.left,
                                right = tackyInstruction.right,
                                dst = tackyInstruction.dst,
                            )
                        }
                    }
                    Tacky.BinaryOperator.Modulo ->
                        idivInstructions(
                            registerValue = Assembly.RegisterValue.Dx,
                            left = tackyInstruction.left,
                            right = tackyInstruction.right,
                            dst = tackyInstruction.dst,
                        )

                    Tacky.BinaryOperator.LessThan ->
                        cmpInstructions(
                            conditionalOperator = if (tackyInstruction.left.signed() &&
                                tackyInstruction.left.assemblyType() != Assembly.Type.Double
                            ) {
                                Assembly.ConditionalOperator.LessThan
                            } else {
                                Assembly.ConditionalOperator.Below
                            },
                            left = tackyInstruction.left,
                            right = tackyInstruction.right,
                            dst = tackyInstruction.dst,
                        )

                    Tacky.BinaryOperator.LessThanOrEqual ->
                        cmpInstructions(
                            conditionalOperator = if (tackyInstruction.left.signed() &&
                                tackyInstruction.left.assemblyType() != Assembly.Type.Double
                            ) {
                                Assembly.ConditionalOperator.LessThanOrEqual
                            } else {
                                Assembly.ConditionalOperator.BelowOrEqual
                            },
                            left = tackyInstruction.left,
                            right = tackyInstruction.right,
                            dst = tackyInstruction.dst,
                        )

                    Tacky.BinaryOperator.GreaterThan ->
                        cmpInstructions(
                            conditionalOperator = if (tackyInstruction.left.signed() &&
                                tackyInstruction.left.assemblyType() != Assembly.Type.Double
                            ) {
                                Assembly.ConditionalOperator.GreaterThan
                            } else {
                                Assembly.ConditionalOperator.Above
                            },
                            left = tackyInstruction.left,
                            right = tackyInstruction.right,
                            dst = tackyInstruction.dst,
                        )

                    Tacky.BinaryOperator.GreaterThanOrEqual ->
                        cmpInstructions(
                            conditionalOperator = if (tackyInstruction.left.signed() &&
                                tackyInstruction.left.assemblyType() != Assembly.Type.Double
                            ) {
                                Assembly.ConditionalOperator.GreaterThanOrEqual
                            } else {
                                Assembly.ConditionalOperator.AboveOrEqual
                            },
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
                if (tackyInstruction.src.assemblyType() == Assembly.Type.Double) {
                    val nanLabel = nextLabel("jump_if_zero_nan")
                    listOf(
                        Assembly.Instruction.Binary(
                            operator = Assembly.BinaryOperator.Xor,
                            type = Assembly.Type.Double,
                            src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                        ),
                        Assembly.Instruction.Cmp(
                            type = Assembly.Type.Double,
                            src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            dst = tackyValueToOperand(tackyInstruction.src),
                        ),
                        Assembly.Instruction.ConditionalJump(
                            operator = Assembly.ConditionalOperator.Parity,
                            target = nanLabel,
                        ),
                        Assembly.Instruction.ConditionalJump(
                            operator = Assembly.ConditionalOperator.Equal,
                            target = tackyInstruction.target,
                        ),
                        Assembly.Instruction.Label(nanLabel),
                    )
                } else {
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
                }

            is Tacky.Instruction.JumpIfNotZero ->
                if (tackyInstruction.src.assemblyType() == Assembly.Type.Double) {
                    listOf(
                        Assembly.Instruction.Binary(
                            operator = Assembly.BinaryOperator.Xor,
                            type = Assembly.Type.Double,
                            src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                        ),
                        Assembly.Instruction.Cmp(
                            type = Assembly.Type.Double,
                            src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            dst = tackyValueToOperand(tackyInstruction.src),
                        ),
                        Assembly.Instruction.ConditionalJump(
                            operator = Assembly.ConditionalOperator.Parity,
                            target = tackyInstruction.target,
                        ),
                        Assembly.Instruction.ConditionalJump(
                            operator = Assembly.ConditionalOperator.NotEqual,
                            target = tackyInstruction.target,
                        ),
                    )
                } else {
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
                }

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

            is Tacky.Instruction.Return -> {
                val returnRegister = if (tackyInstruction.value.assemblyType() == Assembly.Type.Double) {
                    Assembly.RegisterValue.Xmm0
                } else {
                    Assembly.RegisterValue.Ax
                }
                listOf(
                    Assembly.Instruction.Mov(
                        type = tackyInstruction.value.assemblyType(),
                        src = tackyValueToOperand(tackyInstruction.value),
                        dst = Assembly.Operand.Register(returnRegister),
                    ),
                    Assembly.Instruction.Ret,
                )
            }

            is Tacky.Instruction.Call -> functionCall(tackyInstruction)

            is Tacky.Instruction.SignExtend -> listOf(
                Assembly.Instruction.Movsx(
                    src = tackyValueToOperand(tackyInstruction.src),
                    dst = tackyValueToOperand(tackyInstruction.dst),
                ),
            )

            is Tacky.Instruction.ZeroExtend -> listOf(
                Assembly.Instruction.MovZeroExtend(
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

            is Tacky.Instruction.DoubleToInt ->
                listOf(
                    Assembly.Instruction.DoubleToInt(
                        type = tackyInstruction.dst.assemblyType(),
                        src = tackyValueToOperand(tackyInstruction.src),
                        dst = tackyValueToOperand(tackyInstruction.dst),
                    ),
                )
            is Tacky.Instruction.IntToDouble ->
                listOf(
                    Assembly.Instruction.IntToDouble(
                        type = tackyInstruction.src.assemblyType(),
                        src = tackyValueToOperand(tackyInstruction.src),
                        dst = tackyValueToOperand(tackyInstruction.dst),
                    ),
                )
            is Tacky.Instruction.DoubleToUInt -> {
                when (tackyInstruction.dst.assemblyType()) {
                    Assembly.Type.LongWord -> {
                        // unsigned int may not fit into int, so we can't just use double to int with long word type (cvttsd2sil)
                        // we have to convert double to unsigned long first and then truncate it to unsigned int
                        listOf(
                            Assembly.Instruction.DoubleToInt(
                                type = Assembly.Type.QuadWord,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                            ),
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.LongWord,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                        )
                    }
                    Assembly.Type.QuadWord -> {
                        val longMaxPlus1Double = doubleConstantToOperand((ULong.MAX_VALUE + 1u).toDouble(), 8)
                        val outOfLongRangeLabel = nextLabel("double_to_uint_out_of_long_range")
                        val endLabel = nextLabel("double_to_uint_end")
                        listOf(
                            // check if the value is in the range of signed long, by comparing with Long.MAX_VALUE + 1
                            Assembly.Instruction.Cmp(
                                type = Assembly.Type.Double,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = longMaxPlus1Double,
                            ),
                            Assembly.Instruction.ConditionalJump(
                                operator = Assembly.ConditionalOperator.AboveOrEqual,
                                target = outOfLongRangeLabel,
                            ),
                            // if it is in the range, just convert it
                            Assembly.Instruction.DoubleToInt(
                                type = Assembly.Type.QuadWord,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                            Assembly.Instruction.Jump(endLabel),
                            Assembly.Instruction.Label(outOfLongRangeLabel),
                            // if it's not, we subtract Long.MAX_VALUE + 1 from the value, so that it's in the range of signed long
                            // convert it to long and add Long.MAX_VALUE + 1 to get the original value
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.Double,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm1),
                            ),
                            Assembly.Instruction.Binary(
                                operator = Assembly.BinaryOperator.Sub,
                                type = Assembly.Type.Double,
                                src = longMaxPlus1Double,
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm1),
                            ),
                            Assembly.Instruction.DoubleToInt(
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm1),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                            Assembly.Instruction.Binary(
                                operator = Assembly.BinaryOperator.Add,
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Immediate((ULong.MAX_VALUE + 1u).toLong()),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                            Assembly.Instruction.Label(endLabel),
                        )
                    }
                    Assembly.Type.Double -> {
                        error("Bug: unexpected type for dst of DoubleToUInt instruction")
                    }
                }
            }
            is Tacky.Instruction.UIntToDouble -> {
                when (tackyInstruction.src.assemblyType()) {
                    Assembly.Type.LongWord -> {
                        // unsigned int may not fit into int, so we can't just use int to double with long word type (cvtsi2sdq)
                        // we sign extend unsigned int to long first, by moving it into register (this zeroes out upper 4-bytes)
                        // and then convert it to double
                        listOf(
                            Assembly.Instruction.MovZeroExtend(
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                            ),
                            Assembly.Instruction.IntToDouble(
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                        )
                    }
                    Assembly.Type.QuadWord -> {
                        val outOfLongRange = nextLabel("uint_to_double_out_of_long_range")
                        val endLabel = nextLabel("uint_to_double_end")
                        listOf(
                            // checking if the value is in the range of signed long, by signed comparing with 0 with signed conditional jump (jl)
                            Assembly.Instruction.Cmp(
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Immediate(0),
                                dst = tackyValueToOperand(tackyInstruction.src),
                            ),
                            Assembly.Instruction.ConditionalJump(
                                operator = Assembly.ConditionalOperator.LessThan,
                                target = outOfLongRange,
                            ),
                            // if it is in the range, just convert it
                            Assembly.Instruction.IntToDouble(
                                type = Assembly.Type.QuadWord,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                            Assembly.Instruction.Jump(endLabel),
                            Assembly.Instruction.Label(outOfLongRange),
                            // if it's not, we divide it by 2 by shifting right by 1,
                            // and round to odd with (x / 2) | (x & 1)
                            // to avoid double rounding error
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.QuadWord,
                                src = tackyValueToOperand(tackyInstruction.src),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                            ),
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                            ),
                            // shift takes cl register as count, so we have to move 1 to cl
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.LongWord,
                                src = Assembly.Operand.Immediate(1),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Cx),
                            ),
                            Assembly.Instruction.Shift(
                                type = Assembly.Type.QuadWord,
                                shiftType = Assembly.ShiftType.Logical,
                                operator = Assembly.ShiftOperator.Right,
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                            ),
                            Assembly.Instruction.Binary(
                                type = Assembly.Type.QuadWord,
                                operator = Assembly.BinaryOperator.And,
                                src = Assembly.Operand.Immediate(1),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                            ),
                            Assembly.Instruction.Binary(
                                type = Assembly.Type.QuadWord,
                                operator = Assembly.BinaryOperator.Or,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                            ),
                            Assembly.Instruction.IntToDouble(
                                type = Assembly.Type.QuadWord,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            ),
                            // and then getting original value by multiplying by 2
                            Assembly.Instruction.Binary(
                                operator = Assembly.BinaryOperator.Add,
                                type = Assembly.Type.Double,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                                dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                            ),
                            Assembly.Instruction.Mov(
                                type = Assembly.Type.Double,
                                src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                                dst = tackyValueToOperand(tackyInstruction.dst),
                            ),
                            Assembly.Instruction.Label(endLabel),
                        )
                    }
                    Assembly.Type.Double -> error("Bug: unexpected type for src of UIntToDouble instruction")
                }
            }
        }

    private fun tackyValueToOperand(tackyValue: Tacky.Value): Assembly.Operand =
        when (tackyValue) {
            is Tacky.Value.Constant -> {
                when (tackyValue.value) {
                    is AST.IntConstant -> Assembly.Operand.Immediate(tackyValue.value.value.toLong())
                    is AST.LongConstant -> Assembly.Operand.Immediate(tackyValue.value.value)
                    is AST.UIntConstant -> Assembly.Operand.Immediate(tackyValue.value.value.toLong())
                    is AST.ULongConstant -> Assembly.Operand.Immediate(tackyValue.value.value.toLong())
                    is AST.DoubleConstant -> doubleConstantToOperand(tackyValue.value.value, 8)
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

    private fun doubleUnaryNegateInstructions(
        src: Tacky.Value,
        dst: Tacky.Value,
    ): List<Assembly.Instruction> {
        // 16 alignment is required for xorpd instruction
        val minusZero = doubleConstantToOperand(-0.0, 16)
        return listOf(
            Assembly.Instruction.Mov(
                type = Assembly.Type.Double,
                src = tackyValueToOperand(src),
                dst = tackyValueToOperand(dst),
            ),
            Assembly.Instruction.Binary(
                operator = Assembly.BinaryOperator.Xor,
                type = Assembly.Type.Double,
                src = minusZero,
                dst = tackyValueToOperand(dst),
            ),
        )
    }

    private fun doubleLogicalNotInstructions(
        src: Tacky.Value,
        dst: Tacky.Value,
    ): List<Assembly.Instruction> {
        val dstAssembly = tackyValueToOperand(dst)
        val nanLabel = nextLabel("logical_not_nan")
        val endLabel = nextLabel("logical_not_end")
        return listOf(
            Assembly.Instruction.Mov(
                type = Assembly.Type.LongWord,
                src = Assembly.Operand.Immediate(0),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Binary(
                operator = Assembly.BinaryOperator.Xor,
                type = Assembly.Type.Double,
                src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
            ),
            Assembly.Instruction.Cmp(
                type = src.assemblyType(),
                src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm0),
                dst = tackyValueToOperand(src),
            ),
            Assembly.Instruction.ConditionalJump(
                operator = Assembly.ConditionalOperator.Parity,
                target = nanLabel,
            ),
            Assembly.Instruction.Set(
                operator = Assembly.ConditionalOperator.Equal,
                dst = dstAssembly,
            ),
            Assembly.Instruction.Jump(
                target = endLabel,
            ),
            Assembly.Instruction.Label(
                label = nanLabel,
            ),
            Assembly.Instruction.Mov(
                type = Assembly.Type.LongWord,
                src = Assembly.Operand.Immediate(0),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Label(
                label = endLabel,
            ),
        )
    }

    private fun logicalNotInstructions(
        src: Tacky.Value,
        dst: Tacky.Value,
    ): List<Assembly.Instruction> {
        val dstAssembly = tackyValueToOperand(dst)
        return listOf(
            Assembly.Instruction.Cmp(
                type = src.assemblyType(),
                src = tackyValueToOperand(src),
                dst = Assembly.Operand.Immediate(0),
            ),
            Assembly.Instruction.Mov(
                type = dst.assemblyType(),
                src = Assembly.Operand.Immediate(0),
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
    ): List<Assembly.Instruction> = if (left.signed()) {
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
    } else {
        listOf(
            Assembly.Instruction.Mov(
                type = left.assemblyType(),
                src = tackyValueToOperand(left),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
            ),
            Assembly.Instruction.Mov(
                type = left.assemblyType(),
                src = Assembly.Operand.Immediate(0),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
            ),
            Assembly.Instruction.Div(
                type = right.assemblyType(),
                operand = tackyValueToOperand(right),
            ),
            Assembly.Instruction.Mov(
                type = dst.assemblyType(),
                src = Assembly.Operand.Register(registerValue),
                dst = tackyValueToOperand(dst),
            ),
        )
    }

    private fun cmpInstructions(
        conditionalOperator: Assembly.ConditionalOperator,
        left: Tacky.Value,
        right: Tacky.Value,
        dst: Tacky.Value,
    ): List<Assembly.Instruction> {
        val dstAssembly = tackyValueToOperand(dst)
        if (left.assemblyType() != Assembly.Type.Double) {
            return listOf(
                Assembly.Instruction.Mov(
                    type = dst.assemblyType(),
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

        val nanLabel = nextLabel("cmp_nan")
        val endLabel = nextLabel("cmp_end")
        val nanResult = if (conditionalOperator == Assembly.ConditionalOperator.NotEqual) {
            Assembly.Operand.Immediate(1)
        } else {
            Assembly.Operand.Immediate(0)
        }
        return listOf(
            Assembly.Instruction.Mov(
                type = dst.assemblyType(),
                src = Assembly.Operand.Immediate(0),
                dst = dstAssembly,
            ),
            Assembly.Instruction.Cmp(
                type = left.assemblyType(),
                src = tackyValueToOperand(right),
                dst = tackyValueToOperand(left),
            ),
            Assembly.Instruction.ConditionalJump(
                operator = Assembly.ConditionalOperator.Parity,
                target = nanLabel,
            ),
            Assembly.Instruction.Set(
                operator = conditionalOperator,
                dst = dstAssembly,
            ),
            Assembly.Instruction.Jump(
                target = endLabel,
            ),
            Assembly.Instruction.Label(nanLabel),
            Assembly.Instruction.Mov(
                type = dst.assemblyType(),
                src = nanResult,
                dst = dstAssembly,
            ),
            Assembly.Instruction.Label(endLabel),
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
                shiftType = if (left.signed()) Assembly.ShiftType.Arithmetic else Assembly.ShiftType.Logical,
            ),
        )
    }

    private fun functionCall(call: Tacky.Instruction.Call): List<Assembly.Instruction> = buildList {
        val classifiedParameters = classifyParameters(call.arguments) { parameter ->
            parameter.assemblyType()
        }

        // stack must be 16-bytes aligned before issuing a call instruction
        // each stack argument is 8-bytes long, so if number is even, there's nothing to align
        val stackPadding = if (classifiedParameters.stackArguments.size % 2 == 0) {
            0L
        } else {
            8L
        }
        if (stackPadding != 0L) {
            add(
                Assembly.Instruction.Binary(
                    operator = Assembly.BinaryOperator.Sub,
                    type = Assembly.Type.QuadWord,
                    src = Assembly.Operand.Immediate(stackPadding),
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Sp),
                ),
            )
        }

        classifiedParameters.intRegisterArguments.forEachIndexed { index, arg ->
            add(
                Assembly.Instruction.Mov(
                    type = arg.assemblyType(),
                    src = tackyValueToOperand(arg),
                    dst = Assembly.Operand.Register(intArgumentRegisters[index]),
                ),
            )
        }
        classifiedParameters.doubleRegisterArguments.forEachIndexed { index, arg ->
            add(
                Assembly.Instruction.Mov(
                    type = arg.assemblyType(),
                    src = tackyValueToOperand(arg),
                    dst = Assembly.Operand.Register(doubleArgumentRegisters[index]),
                ),
            )
        }
        classifiedParameters.stackArguments.reversed().forEach { arg ->
            // even though our values can be 4-bytes, pushq accepts 8-bytes operands,
            // in case of immediate or register values there's no problem
            // but if the operand is 4-bytes and is the value somewhere in the memory, higher 4-bytes may lie in the inaccessible memory
            // to prevent that, we move the 4-bytes value to the register first and then push the register
            val assemblyArgument = tackyValueToOperand(arg)
            when {
                assemblyArgument is Assembly.Operand.Immediate || assemblyArgument is Assembly.Operand.Register ||
                    arg.assemblyType() == Assembly.Type.QuadWord || arg.assemblyType() == Assembly.Type.Double -> {
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

        val bytesToRemove = classifiedParameters.stackArguments.size * 8 + stackPadding
        if (bytesToRemove != 0L) {
            add(
                Assembly.Instruction.Binary(
                    operator = Assembly.BinaryOperator.Add,
                    type = Assembly.Type.QuadWord,
                    src = Assembly.Operand.Immediate(bytesToRemove),
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Sp),
                ),
            )
        }

        val assemblyDst = tackyValueToOperand(call.dst)
        val returnType = call.dst.assemblyType()
        val returnValueRegister = if (returnType == Assembly.Type.Double) {
            Assembly.RegisterValue.Xmm0
        } else {
            Assembly.RegisterValue.Ax
        }
        add(
            Assembly.Instruction.Mov(
                type = returnType,
                src = Assembly.Operand.Register(returnValueRegister),
                dst = assemblyDst,
            ),
        )
    }

    private fun Tacky.Value.assemblyType() = when (this) {
        is Tacky.Value.Constant -> value.type.toAssemblyType()
        is Tacky.Value.Variable -> symbolTable.objectSymbol(name).type
    }

    private fun Tacky.Value.signed() = when (this) {
        is Tacky.Value.Constant -> value.type.signed()
        is Tacky.Value.Variable -> symbolTable.objectSymbol(name).signed
    }
}

fun Assembly.Operand.isMemory() = when (this) {
    is Assembly.Operand.Data -> true
    is Assembly.Operand.Immediate -> false
    is Assembly.Operand.PseudoIdentifier -> true
    is Assembly.Operand.Register -> false
    is Assembly.Operand.Stack -> true
}
