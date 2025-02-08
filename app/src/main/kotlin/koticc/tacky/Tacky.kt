package koticc.tacky

import koticc.ast.AST
import koticc.ast.LabelName
import koticc.ast.Type
import koticc.semantic.InitialConstantValue
import koticc.semantic.SymbolTable

object Tacky {
    data class Program(
        val topLevel: List<TopLevel>,
        val symbolTable: SymbolTable,
    )

    sealed interface TopLevel {
        data class FunctionDefinition(val value: Tacky.FunctionDefinition) : TopLevel
        data class StaticVariable(val value: Tacky.StaticVariable) : TopLevel
    }

    data class FunctionDefinition(
        val name: String,
        val parameters: List<String>,
        val global: Boolean,
        val body: List<Instruction>,
    )

    data class StaticVariable(
        val name: String,
        val global: Boolean,
        val type: Type.Data,
        val initialValue: InitialConstantValue,
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

        data class Call(
            val name: String,
            val arguments: List<Value>,
            val dst: Value,
        ) : Instruction

        data class SignExtend(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class ZeroExtend(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class Truncate(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class DoubleToInt(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class IntToDouble(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class DoubleToUInt(
            val src: Value,
            val dst: Value,
        ) : Instruction

        data class UIntToDouble(
            val src: Value,
            val dst: Value,
        ) : Instruction
    }

    sealed interface Value {
        data class Constant(val value: AST.Constant) : Value

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
