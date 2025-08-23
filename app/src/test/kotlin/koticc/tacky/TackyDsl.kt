@file:Suppress("ktlint:standard:function-naming")

package koticc.tacky

import koticc.ast.AST
import koticc.ast.LabelName
import koticc.ast.Type
import koticc.semantic.InitialConstantValue
import koticc.semantic.SymbolTable

fun tackyProgram(block: TackyProgramBuilder.() -> Unit): Tacky.Program = TackyProgramBuilder().apply(block).build()

class TackyProgramBuilder {
    private val topLevel = mutableListOf<Tacky.TopLevel>()
    var symbolTable: SymbolTable = emptyMap()

    fun function(name: String, vararg parameters: String, block: FunctionBuilder.() -> Unit) {
        topLevel += Tacky.TopLevel.FunctionDefinition(FunctionBuilder(name, parameters, global = true).apply(block).build())
    }

    fun nonGlobalFunction(name: String, vararg parameters: String, block: FunctionBuilder.() -> Unit) {
        topLevel += Tacky.TopLevel.FunctionDefinition(FunctionBuilder(name, parameters, global = false).apply(block).build())
    }

    fun staticVariable(name: String, global: Boolean, initialValue: Int) {
        topLevel += Tacky.TopLevel.StaticVariable(
            Tacky.StaticVariable(
                name,
                global = global,
                initialValues = listOf(
                    if (initialValue != 0) {
                        InitialConstantValue.Int(initialValue)
                    } else {
                        InitialConstantValue.Zero(bytes = 4)
                    },
                ),
                type = Type.Int,
            ),
        )
    }

    fun staticVariable(name: String, global: Boolean, initialValue: Long) {
        topLevel += Tacky.TopLevel.StaticVariable(
            Tacky.StaticVariable(
                name,
                global = global,
                initialValues = listOf(
                    if (initialValue != 0L) {
                        InitialConstantValue.Long(initialValue)
                    } else {
                        InitialConstantValue.Zero(bytes = 8)
                    },
                ),
                type = Type.Long,
            ),
        )
    }

    fun build(): Tacky.Program = Tacky.Program(topLevel, symbolTable)
}

val Int.t
    get() = Tacky.Value.Constant(AST.IntConstant(this))

val UInt.t
    get() = Tacky.Value.Constant(AST.UIntConstant(this))

val Long.t
    get() = Tacky.Value.Constant(AST.LongConstant(this))

val ULong.t
    get() = Tacky.Value.Constant(AST.ULongConstant(this))

val Double.t
    get() = Tacky.Value.Constant(AST.DoubleConstant(this))

val String.t
    get() = Tacky.Value.Variable(this)

class FunctionBuilder(private val name: String, private val parameters: Array<out String>, private val global: Boolean) {
    private val instructions = mutableListOf<Tacky.Instruction>()

    fun assign(dst: String, builder: TackyInstructionWithDstBuilder) = instructions.add(builder.build(Tacky.Value.Variable(dst)))

    fun assign(dst: String, value: Tacky.Value) = instructions.add(Tacky.Instruction.Copy(value, Tacky.Value.Variable(dst)))

    fun return_(value: Tacky.Value) = instructions.add(Tacky.Instruction.Return(value))

    fun label(name: String) = instructions.add(Tacky.Instruction.Label(LabelName(name)))

    fun jump(target: String) = instructions.add(Tacky.Instruction.Jump(LabelName(target)))

    fun jumpIfZero(src: Tacky.Value, target: String) = instructions.add(Tacky.Instruction.JumpIfZero(src, LabelName(target)))

    fun jumpIfNotZero(src: Tacky.Value, target: String) = instructions.add(Tacky.Instruction.JumpIfNotZero(src, LabelName(target)))

    fun signExtend(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.SignExtend(src, Tacky.Value.Variable(dst)))

    fun zeroExtend(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.ZeroExtend(src, Tacky.Value.Variable(dst)))

    fun truncate(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.Truncate(src, Tacky.Value.Variable(dst)))

    fun doubleToInt(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.DoubleToInt(src, Tacky.Value.Variable(dst)))

    fun intToDouble(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.IntToDouble(src, Tacky.Value.Variable(dst)))

    fun doubleToUInt(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.DoubleToUInt(src, Tacky.Value.Variable(dst)))

    fun uIntToDouble(src: Tacky.Value, dst: String) = instructions.add(Tacky.Instruction.UIntToDouble(src, Tacky.Value.Variable(dst)))

    fun build() = Tacky.FunctionDefinition(name, parameters.toList(), global, instructions)
}

interface TackyInstructionWithDstBuilder {
    fun build(dst: Tacky.Value): Tacky.Instruction
}

class TackyUnaryOperatorBuilder(private val operator: Tacky.UnaryOperator, private val src: Tacky.Value) : TackyInstructionWithDstBuilder {
    override fun build(dst: Tacky.Value): Tacky.Instruction = Tacky.Instruction.Unary(operator = operator, src = src, dst = dst)
}

operator fun Tacky.Value.unaryMinus() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.Negate, this)

operator fun Tacky.Value.not() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.LogicalNegate, this)

fun Tacky.Value.complement() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.Complement, this)

class TackyBinaryOperatorBuilder(val operator: Tacky.BinaryOperator, val left: Tacky.Value, val right: Tacky.Value) : TackyInstructionWithDstBuilder {
    override fun build(dst: Tacky.Value): Tacky.Instruction = Tacky.Instruction.Binary(operator = operator, left = left, right = right, dst = dst)
}

operator fun Tacky.Value.plus(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Add, this, other)

operator fun Tacky.Value.minus(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Subtract, this, other)

operator fun Tacky.Value.times(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Multiply, this, other)

operator fun Tacky.Value.div(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Divide, this, other)

operator fun Tacky.Value.rem(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Modulo, this, other)

infix fun Tacky.Value.eq(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.Equal, this, other)

infix fun Tacky.Value.neq(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.NotEqual, this, other)

infix fun Tacky.Value.lt(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.LessThan, this, other)

infix fun Tacky.Value.lte(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.LessThanOrEqual, this, other)

infix fun Tacky.Value.gt(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.GreaterThan, this, other)

infix fun Tacky.Value.gte(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.GreaterThanOrEqual, this, other)

infix fun Tacky.Value.and(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.BitwiseAnd, this, other)

infix fun Tacky.Value.or(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.BitwiseOr, this, other)

infix fun Tacky.Value.xor(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.BitwiseXor, this, other)

infix fun Tacky.Value.shl(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.ShiftLeft, this, other)

infix fun Tacky.Value.shr(other: Tacky.Value) = TackyBinaryOperatorBuilder(Tacky.BinaryOperator.ShiftRight, this, other)

fun call(name: String, vararg arguments: Tacky.Value) = TackyFunctionCallBuilder(name, arguments.toList())

class TackyFunctionCallBuilder(private val name: String, private val arguments: List<Tacky.Value>) : TackyInstructionWithDstBuilder {
    override fun build(dst: Tacky.Value) = Tacky.Instruction.Call(name, arguments, dst)
}
