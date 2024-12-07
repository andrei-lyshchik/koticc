package koticc

object Assembly {
    data class Program(
        val functionDefinitions: List<FunctionDefinition>,
    )

    data class FunctionDefinition(
        val name: String,
        val body: List<Instruction>,
    )

    sealed interface Instruction {
        data class Mov(
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class Unary(
            val operator: UnaryOperator,
            val operand: Operand,
        ) : Instruction

        data class Binary(
            val operator: BinaryOperator,
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class AllocateStack(
            val size: Int,
        ) : Instruction

        data class DeallocateStack(
            val size: Int,
        ) : Instruction

        data class Push(
            val operand: Operand,
        ) : Instruction

        data class Call(
            val name: String,
        ) : Instruction

        data object Cdq : Instruction

        data class Idiv(
            val operand: Operand,
        ) : Instruction

        // always uses %cl as count
        data class Shift(
            val operator: ShiftOperator,
            val dst: Operand,
        ) : Instruction

        data class Cmp(
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class Set(
            val operator: ConditionalOperator,
            val dst: Operand,
        ) : Instruction

        data class ConditionalJump(
            val operator: ConditionalOperator,
            val target: LabelName,
        ) : Instruction

        data class Jump(
            val target: LabelName,
        ) : Instruction

        data class Label(
            val label: LabelName,
        ) : Instruction

        data object Ret : Instruction
    }

    sealed interface Operand {
        data class Register(val value: RegisterValue) : Operand

        data class Immediate(val value: Int) : Operand

        data class Stack(val offset: Int) : Operand

        data class PseudoIdentifier(val name: String) : Operand
    }

    enum class RegisterValue {
        Ax,
        Cx,
        Dx,
        Di,
        Si,
        R8,
        R9,
        R10,
        R11,
    }

    enum class UnaryOperator {
        Neg,
        Not,
    }

    enum class BinaryOperator {
        Add,
        Sub,
        Mul,
        And,
        Or,
        Xor,
    }

    enum class ShiftOperator {
        Left,
        Right,
    }

    enum class ConditionalOperator {
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        Equal,
        NotEqual,
    }
}
