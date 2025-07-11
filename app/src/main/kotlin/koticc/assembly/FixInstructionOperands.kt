package koticc.assembly

fun fixInstructionOperands(instruction: Assembly.Instruction): List<Assembly.Instruction> = when (instruction) {
    is Assembly.Instruction.Binary -> {
        if (instruction.type == Assembly.Type.Double) {
            fixDoubleBinary(instruction)
        } else {
            fixIntegerBinary(instruction)
        }
    }

    is Assembly.Instruction.Cdq -> listOf(instruction)
    is Assembly.Instruction.Cmp -> {
        if (instruction.type == Assembly.Type.Double) {
            fixDoubleCmp(instruction)
        } else {
            fixIntegerCmp(instruction)
        }
    }

    is Assembly.Instruction.ConditionalJump -> listOf(instruction)
    is Assembly.Instruction.Idiv -> {
        if (instruction.operand is Assembly.Operand.Immediate) {
            listOf(
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = instruction.operand,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
                Assembly.Instruction.Idiv(
                    type = instruction.type,
                    operand = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
            )
        } else {
            listOf(instruction)
        }
    }
    is Assembly.Instruction.Div -> {
        if (instruction.operand is Assembly.Operand.Immediate) {
            listOf(
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = instruction.operand,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
                Assembly.Instruction.Div(
                    type = instruction.type,
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
        val fixRegister = if (instruction.type == Assembly.Type.Double) {
            Assembly.Operand.Register(Assembly.RegisterValue.Xmm15)
        } else {
            Assembly.Operand.Register(Assembly.RegisterValue.R10)
        }
        if (instruction.src.isMemory() && instruction.dst.isMemory()) {
            listOf(
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = instruction.src,
                    dst = fixRegister,
                ),
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = fixRegister,
                    dst = instruction.dst,
                ),
            )
        } else if (instruction.src is Assembly.Operand.Immediate && !instruction.src.isInt() && instruction.dst.isMemory()) {
            listOf(
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = instruction.src,
                    dst = fixRegister,
                ),
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = fixRegister,
                    dst = instruction.dst,
                ),
            )
        } else {
            listOf(instruction)
        }
    }

    is Assembly.Instruction.Movsx -> {
        buildList {
            val fixedSrc = if (instruction.src is Assembly.Operand.Immediate) {
                add(
                    Assembly.Instruction.Mov(
                        type = Assembly.Type.LongWord,
                        src = instruction.src,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    ),
                )
                Assembly.Operand.Register(Assembly.RegisterValue.R10)
            } else {
                instruction.src
            }

            if (instruction.dst.isMemory()) {
                add(
                    Assembly.Instruction.Movsx(
                        src = fixedSrc,
                        dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                    ),
                )
                add(
                    Assembly.Instruction.Mov(
                        type = Assembly.Type.QuadWord,
                        src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                        dst = instruction.dst,
                    ),
                )
            } else {
                add(
                    Assembly.Instruction.Movsx(
                        src = fixedSrc,
                        dst = instruction.dst,
                    ),
                )
            }
        }
    }
    is Assembly.Instruction.MovZeroExtend -> {
        if (instruction.dst is Assembly.Operand.Register) {
            listOf(
                Assembly.Instruction.Mov(
                    type = Assembly.Type.LongWord,
                    src = instruction.src,
                    dst = instruction.dst,
                ),
            )
        } else {
            listOf(
                Assembly.Instruction.Mov(
                    type = Assembly.Type.LongWord,
                    src = instruction.src,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                ),
                Assembly.Instruction.Mov(
                    type = Assembly.Type.QuadWord,
                    src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                    dst = instruction.dst,
                ),
            )
        }
    }
    Assembly.Instruction.Ret -> listOf(instruction)
    is Assembly.Instruction.Set -> listOf(instruction)
    is Assembly.Instruction.Shift -> listOf(instruction)
    is Assembly.Instruction.Unary -> listOf(instruction)
    is Assembly.Instruction.Call -> listOf(instruction)
    is Assembly.Instruction.Push -> {
        if (instruction.operand is Assembly.Operand.Immediate && !instruction.operand.isInt()) {
            listOf(
                Assembly.Instruction.Mov(
                    type = Assembly.Type.QuadWord,
                    src = instruction.operand,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
                Assembly.Instruction.Push(
                    operand = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
            )
        } else {
            listOf(instruction)
        }
    }

    is Assembly.Instruction.IntToDouble -> {
        val (instructionWithFixedSrc, srcCopyInstructions) = if (instruction.src is Assembly.Operand.Immediate) {
            instruction.copy(src = Assembly.Operand.Register(Assembly.RegisterValue.R11)) to listOf(
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = instruction.src,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                ),
            )
        } else {
            instruction to emptyList<Assembly.Instruction>()
        }
        val instructions = if (instructionWithFixedSrc.dst !is Assembly.Operand.Register) {
            listOf(
                Assembly.Instruction.IntToDouble(
                    type = instructionWithFixedSrc.type,
                    src = instructionWithFixedSrc.src,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
                ),
                Assembly.Instruction.Mov(
                    type = Assembly.Type.Double,
                    src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
                    dst = instructionWithFixedSrc.dst,
                ),
            )
        } else {
            listOf(
                instructionWithFixedSrc,
            )
        }
        srcCopyInstructions + instructions
    }
    is Assembly.Instruction.DoubleToInt -> {
        if (instruction.dst !is Assembly.Operand.Register) {
            listOf(
                Assembly.Instruction.DoubleToInt(
                    type = instruction.type,
                    src = instruction.src,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                ),
                Assembly.Instruction.Mov(
                    type = instruction.type,
                    src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                    dst = instruction.dst,
                ),
            )
        } else {
            listOf(
                instruction,
            )
        }
    }

    is Assembly.Instruction.LoadEffectiveAddress -> {
        if (instruction.dst !is Assembly.Operand.Register) {
            listOf(
                Assembly.Instruction.LoadEffectiveAddress(
                    src = instruction.src,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                ),
                Assembly.Instruction.Mov(
                    type = Assembly.Type.QuadWord,
                    src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                    dst = instruction.dst,
                ),
            )
        } else {
            listOf(instruction)
        }
    }
}

private fun fixIntegerBinary(
    instruction: Assembly.Instruction.Binary,
): List<Assembly.Instruction> {
    val (instructionWithFixedSrc, srcCopyInstructions) = if (instruction.src is Assembly.Operand.Immediate && !instruction.src.isInt()) {
        instruction.copy(src = Assembly.Operand.Register(Assembly.RegisterValue.R10)) to listOf(
            Assembly.Instruction.Mov(
                type = Assembly.Type.QuadWord,
                src = instruction.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
            ),
        )
    } else {
        instruction to emptyList()
    }
    val instructions = if (instructionWithFixedSrc.operator == Assembly.BinaryOperator.Mul && instructionWithFixedSrc.dst.isMemory()) {
        listOf(
            Assembly.Instruction.Mov(
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.dst,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
            ),
            Assembly.Instruction.Binary(
                operator = instructionWithFixedSrc.operator,
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
            ),
            Assembly.Instruction.Mov(
                type = instructionWithFixedSrc.type,
                src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                dst = instructionWithFixedSrc.dst,
            ),
        )
    } else if (instructionWithFixedSrc.src.isMemory() && instructionWithFixedSrc.dst.isMemory()) {
        listOf(
            Assembly.Instruction.Mov(
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
            ),
            Assembly.Instruction.Binary(
                operator = instructionWithFixedSrc.operator,
                type = instructionWithFixedSrc.type,
                src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                dst = instructionWithFixedSrc.dst,
            ),
        )
    } else {
        listOf(instructionWithFixedSrc)
    }
    return srcCopyInstructions + instructions
}

private fun fixDoubleBinary(
    instruction: Assembly.Instruction.Binary,
): List<Assembly.Instruction> {
    if (instruction.dst is Assembly.Operand.Register) {
        return listOf(instruction)
    }

    return listOf(
        Assembly.Instruction.Mov(
            type = instruction.type,
            src = instruction.dst,
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
        ),
        Assembly.Instruction.Binary(
            operator = instruction.operator,
            type = instruction.type,
            src = instruction.src,
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
        ),
        Assembly.Instruction.Mov(
            type = instruction.type,
            src = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
            dst = instruction.dst,
        ),
    )
}

private fun fixDoubleCmp(
    instruction: Assembly.Instruction.Cmp,
): List<Assembly.Instruction> {
    if (instruction.dst is Assembly.Operand.Register) {
        return listOf(instruction)
    }

    return listOf(
        Assembly.Instruction.Mov(
            type = instruction.type,
            src = instruction.dst,
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
        ),
        Assembly.Instruction.Cmp(
            type = instruction.type,
            src = instruction.src,
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Xmm15),
        ),
    )
}

private fun fixIntegerCmp(
    instruction: Assembly.Instruction.Cmp,
): List<Assembly.Instruction> {
    val (instructionWithFixedSrc, srcCopyInstructions) = if (instruction.src is Assembly.Operand.Immediate && !instruction.src.isInt()) {
        instruction.copy(src = Assembly.Operand.Register(Assembly.RegisterValue.R11)) to listOf(
            Assembly.Instruction.Mov(
                type = Assembly.Type.QuadWord,
                src = instruction.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
            ),
        )
    } else {
        instruction to emptyList()
    }
    val instructions = if (instructionWithFixedSrc.src.isMemory() && instructionWithFixedSrc.dst.isMemory()) {
        listOf(
            Assembly.Instruction.Mov(
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
            ),
            Assembly.Instruction.Cmp(
                type = instructionWithFixedSrc.type,
                src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                dst = instructionWithFixedSrc.dst,
            ),
        )
    } else if (instructionWithFixedSrc.dst is Assembly.Operand.Immediate) {
        listOf(
            Assembly.Instruction.Mov(
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.dst,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
            ),
            Assembly.Instruction.Cmp(
                type = instructionWithFixedSrc.type,
                src = instructionWithFixedSrc.src,
                dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
            ),
        )
    } else {
        listOf(instructionWithFixedSrc)
    }
    return srcCopyInstructions + instructions
}
