package koticc.assembly

class PseudoIdentifierReplacer(private val symbolTable: BackendSymbolTable) {
    private var currentStackBytes: Int = 0
    private val stackOffsets = mutableMapOf<String, Int>()

    fun replace(instructions: List<Assembly.Instruction>): List<Assembly.Instruction> {
        val result = ArrayList<Assembly.Instruction>(instructions.size + 1)
        result.add(
            Assembly.Instruction.Binary(
                operator = Assembly.BinaryOperator.Sub,
                type = Assembly.Type.QuadWord,
                src = Assembly.Operand.Immediate(0),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Sp),
            ),
        )
        for (instruction in instructions) {
            val replacedInstruction =
                when (instruction) {
                    is Assembly.Instruction.Mov -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Mov(instruction.type, src, dst)
                    }
                    is Assembly.Instruction.MovZeroExtend -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.MovZeroExtend(src, dst)
                    }

                    is Assembly.Instruction.Movsx -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Movsx(src, dst)
                    }

                    is Assembly.Instruction.Binary -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Binary(instruction.operator, instruction.type, src, dst)
                    }

                    is Assembly.Instruction.Cdq -> instruction
                    is Assembly.Instruction.Cmp -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Cmp(instruction.type, src, dst)
                    }

                    is Assembly.Instruction.ConditionalJump -> instruction
                    is Assembly.Instruction.Idiv -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Idiv(instruction.type, operand)
                    }
                    is Assembly.Instruction.Div -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Div(instruction.type, operand)
                    }

                    is Assembly.Instruction.Jump -> instruction
                    is Assembly.Instruction.Label -> instruction
                    Assembly.Instruction.Ret -> instruction
                    is Assembly.Instruction.Set -> {
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Set(instruction.operator, dst)
                    }

                    is Assembly.Instruction.Shift -> {
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Shift(instruction.type, instruction.shiftType, instruction.operator, dst)
                    }

                    is Assembly.Instruction.Unary -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Unary(instruction.type, instruction.operator, operand)
                    }

                    is Assembly.Instruction.Call -> instruction
                    is Assembly.Instruction.Push -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Push(operand)
                    }

                    is Assembly.Instruction.IntToDouble -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.IntToDouble(instruction.type, src, dst)
                    }
                    is Assembly.Instruction.DoubleToInt -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.DoubleToInt(instruction.type, src, dst)
                    }

                    is Assembly.Instruction.LoadEffectiveAddress -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.LoadEffectiveAddress(src, dst)
                    }
                }
            result.add(replacedInstruction)
        }
        // %rsp must be aligned to 16 bytes, so that System V ABI is followed
        val alignedStackBytes = if (currentStackBytes % 16 == 0) {
            currentStackBytes
        } else {
            val padding = 16 - currentStackBytes % 16
            currentStackBytes + padding
        }
        result[0] = Assembly.Instruction.Binary(
            operator = Assembly.BinaryOperator.Sub,
            type = Assembly.Type.QuadWord,
            src = Assembly.Operand.Immediate(alignedStackBytes.toLong()),
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Sp),
        )
        return result
    }

    private fun replace(
        operand: Assembly.Operand,
    ): Assembly.Operand = when (operand) {
        is Assembly.Operand.PseudoIdentifier -> {
            val size = symbolTable.objectSymbol(operand.name).type.byteSize
            val stackOffset = stackOffsets.getOrPut(operand.name) {
                currentStackBytes += size
                if (currentStackBytes % size != 0) {
                    // f.ex. in case of quadwords - 8 bytes - we need to align their offset
                    // so that the offset is divisible by 8
                    currentStackBytes += size - currentStackBytes % size
                }
                -currentStackBytes
            }
            Assembly.Operand.Memory(Assembly.RegisterValue.Bp, stackOffset)
        }

        is Assembly.Operand.Register -> operand
        is Assembly.Operand.Immediate -> operand
        is Assembly.Operand.Data -> operand
        is Assembly.Operand.Memory -> operand
    }
}
