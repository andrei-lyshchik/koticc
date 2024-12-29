package koticc.assembly

class PseudoIdentifierReplacer {
    fun replace(instructions: List<Assembly.Instruction>): List<Assembly.Instruction> {
        val stackOffsets = mutableMapOf<String, Int>()
        val result = ArrayList<Assembly.Instruction>(instructions.size + 1)
        result.add(Assembly.Instruction.AllocateStack(0))
        for (instruction in instructions) {
            val replacedInstruction =
                when (instruction) {
                    is Assembly.Instruction.Mov -> {
                        val src = replace(stackOffsets, instruction.src)
                        val dst = replace(stackOffsets, instruction.dst)
                        Assembly.Instruction.Mov(src, dst)
                    }

                    is Assembly.Instruction.AllocateStack -> instruction
                    is Assembly.Instruction.Binary -> {
                        val src = replace(stackOffsets, instruction.src)
                        val dst = replace(stackOffsets, instruction.dst)
                        Assembly.Instruction.Binary(instruction.operator, src, dst)
                    }

                    Assembly.Instruction.Cdq -> instruction
                    is Assembly.Instruction.Cmp -> {
                        val src = replace(stackOffsets, instruction.src)
                        val dst = replace(stackOffsets, instruction.dst)
                        Assembly.Instruction.Cmp(src, dst)
                    }

                    is Assembly.Instruction.ConditionalJump -> instruction
                    is Assembly.Instruction.Idiv -> {
                        val operand = replace(stackOffsets, instruction.operand)
                        Assembly.Instruction.Idiv(operand)
                    }

                    is Assembly.Instruction.Jump -> instruction
                    is Assembly.Instruction.Label -> instruction
                    Assembly.Instruction.Ret -> instruction
                    is Assembly.Instruction.Set -> {
                        val dst = replace(stackOffsets, instruction.dst)
                        Assembly.Instruction.Set(instruction.operator, dst)
                    }

                    is Assembly.Instruction.Shift -> {
                        val dst = replace(stackOffsets, instruction.dst)
                        Assembly.Instruction.Shift(instruction.operator, dst)
                    }

                    is Assembly.Instruction.Unary -> {
                        val operand = replace(stackOffsets, instruction.operand)
                        Assembly.Instruction.Unary(instruction.operator, operand)
                    }

                    is Assembly.Instruction.Call -> instruction
                    is Assembly.Instruction.DeallocateStack -> instruction
                    is Assembly.Instruction.Push -> {
                        val operand = replace(stackOffsets, instruction.operand)
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

    private fun replace(
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
}
