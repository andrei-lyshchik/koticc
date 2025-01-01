package koticc.assembly

class PseudoIdentifierReplacer(private val symbolTable: BackendSymbolTable) {
    private var currentStackBytes: Int = 0
    private val stackOffsets = mutableMapOf<String, Int>()

    fun replace(instructions: List<Assembly.Instruction>): List<Assembly.Instruction> {
        val result = ArrayList<Assembly.Instruction>(instructions.size + 1)
        result.add(Assembly.Instruction.AllocateStack(0))
        for (instruction in instructions) {
            val replacedInstruction =
                when (instruction) {
                    is Assembly.Instruction.Mov -> {
                        val src = replace(instruction.src)
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Mov(instruction.type, src, dst)
                    }

                    is Assembly.Instruction.AllocateStack -> instruction
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

                    is Assembly.Instruction.Jump -> instruction
                    is Assembly.Instruction.Label -> instruction
                    Assembly.Instruction.Ret -> instruction
                    is Assembly.Instruction.Set -> {
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Set(instruction.operator, dst)
                    }

                    is Assembly.Instruction.Shift -> {
                        val dst = replace(instruction.dst)
                        Assembly.Instruction.Shift(instruction.type, instruction.operator, dst)
                    }

                    is Assembly.Instruction.Unary -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Unary(instruction.type, instruction.operator, operand)
                    }

                    is Assembly.Instruction.Call -> instruction
                    is Assembly.Instruction.DeallocateStack -> instruction
                    is Assembly.Instruction.Push -> {
                        val operand = replace(instruction.operand)
                        Assembly.Instruction.Push(operand)
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
        result[0] = Assembly.Instruction.AllocateStack(alignedStackBytes)
        return result
    }

    private fun replace(
        operand: Assembly.Operand,
    ): Assembly.Operand =
        when (operand) {
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
                Assembly.Operand.Stack(stackOffset)
            }

            is Assembly.Operand.Register -> operand
            is Assembly.Operand.Immediate -> operand
            is Assembly.Operand.Stack -> operand
            is Assembly.Operand.Data -> operand
        }
}