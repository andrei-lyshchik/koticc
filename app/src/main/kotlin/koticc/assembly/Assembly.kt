package koticc.assembly

import koticc.ast.LabelName
import koticc.semantic.InitialConstantValue

object Assembly {
    enum class Type(val byteSize: Int) {
        LongWord(4),
        QuadWord(8),
        Double(8),
    }

    data class Program(
        val topLevel: List<TopLevel>,
    )

    sealed interface TopLevel {
        data class FunctionDefinition(val value: Assembly.FunctionDefinition) : TopLevel
        data class StaticVariable(val value: Assembly.StaticVariable) : TopLevel
        data class StaticConstant(val value: Assembly.StaticConstant) : TopLevel
    }

    data class FunctionDefinition(
        val name: String,
        val global: Boolean,
        val body: List<Instruction>,
    )

    data class StaticVariable(
        val name: String,
        val global: Boolean,
        val initialValue: InitialConstantValue,
        val alignment: Int,
    )

    data class StaticConstant(
        val name: String,
        val alignment: Int,
        val value: InitialConstantValue,
    )

    sealed interface Instruction {
        data class Mov(
            val type: Type,
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class Movsx(
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class MovZeroExtend(
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class Unary(
            val type: Assembly.Type,
            val operator: UnaryOperator,
            val operand: Operand,
        ) : Instruction

        data class Binary(
            val operator: BinaryOperator,
            val type: Type,
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class Push(
            val operand: Operand,
        ) : Instruction

        data class Call(
            val name: String,
        ) : Instruction

        data class Cdq(val type: Type) : Instruction

        data class Idiv(
            val type: Type,
            val operand: Operand,
        ) : Instruction

        data class Div(
            val type: Type,
            val operand: Operand,
        ) : Instruction

        // always uses %cl as count
        data class Shift(
            val type: Type,
            val shiftType: ShiftType,
            val operator: ShiftOperator,
            val dst: Operand,
        ) : Instruction

        data class Cmp(
            val type: Type,
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

        data class DoubleToInt(
            val type: Type,
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class IntToDouble(
            val type: Type,
            val src: Operand,
            val dst: Operand,
        ) : Instruction

        data class LoadEffectiveAddress(
            val src: Operand,
            val dst: Operand,
        ) : Instruction
    }

    sealed interface Operand {
        data class Register(val value: RegisterValue) : Operand

        data class Immediate(val value: Long) : Operand {
            fun isInt(): Boolean = value >= Int.MIN_VALUE && value <= Int.MAX_VALUE
        }

        data class Data(val name: String) : Operand

        data class Memory(val register: RegisterValue, val offset: Int) : Operand

        data class PseudoIdentifier(val name: String) : Operand
    }

    enum class RegisterValue {
        Ax,
        Bp,
        Cx,
        Dx,
        Di,
        Si,
        Sp,
        R8,
        R9,
        R10,
        R11,

        Xmm0,
        Xmm1,
        Xmm2,
        Xmm3,
        Xmm4,
        Xmm5,
        Xmm6,
        Xmm7,
        Xmm14,
        Xmm15,
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
        DivDouble,
    }

    enum class ShiftOperator {
        Left,
        Right,
    }

    enum class ShiftType {
        Logical,
        Arithmetic,
    }

    enum class ConditionalOperator {
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        Equal,
        NotEqual,
        Below,
        BelowOrEqual,
        Above,
        AboveOrEqual,
        Parity,
    }
}
