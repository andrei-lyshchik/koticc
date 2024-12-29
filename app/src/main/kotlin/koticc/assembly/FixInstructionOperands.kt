package koticc.assembly

fun fixInstructionOperands(instruction: Assembly.Instruction): List<Assembly.Instruction> =
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
