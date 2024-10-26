package koticc

object Tacky {
    data class Program(
        val functionDefinition: FunctionDefinition,
    )

    data class FunctionDefinition(
        val name: String,
        val body: List<Instruction>,
    )

    sealed interface Instruction {
        data class Unary(
            val operator: UnaryOperator,
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class Binary(
            val operator: BinaryOperator,
            val left: Value,
            val right: Value,
            val dst: Value,
        ) : Instruction

        data class Copy(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class JumpIfZero(
            val src: Value,
            val target: LabelName,
        ) : Instruction

        data class JumpIfNotZero(
            val src: Value,
            val target: LabelName,
        ) : Instruction

        data class Jump(
            val target: LabelName,
        ) : Instruction

        data class Label(
            val label: LabelName,
        ) : Instruction

        data class Return(
            val value: Value,
        ) : Instruction
    }

    sealed interface Value {
        data class IntConstant(val value: Int) : Value

        data class Variable(val name: String) : Value
    }

    enum class UnaryOperator {
        Negate,
        Complement,
        LogicalNegate,
    }

    enum class BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
        Equal,
        NotEqual,
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        ShiftLeft,
        ShiftRight,
    }
}
