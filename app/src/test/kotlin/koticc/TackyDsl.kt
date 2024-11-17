@file:Suppress("ktlint:standard:function-naming")

package koticc

fun tackyProgram(block: TackyProgramBuilder.() -> Unit): Tacky.Program = TackyProgramBuilder().apply(block).build()

class TackyProgramBuilder {
    private val functions = mutableListOf<Tacky.FunctionDefinition>()

    fun function(name: String, block: FunctionBuilder.() -> Unit) {
        functions += FunctionBuilder(name).apply(block).build()
    }

    fun build(): Tacky.Program {
        require(functions.size == 1) { "Tacky program must have exactly one function for now" }
        return Tacky.Program(functions.first())
    }
}

val Int.t
    get() = Tacky.Value.IntConstant(this)

val String.t
    get() = Tacky.Value.Variable(this)

infix fun Tacky.Value.to(dst: Tacky.Value) = Tacky.Instruction.Copy(this, dst)

class FunctionBuilder(val name: String) {
    private val instructions = mutableListOf<Tacky.Instruction>()

    fun i(instruction: Tacky.Instruction) = instructions.add(instruction)

    fun return_(value: Tacky.Value) = instructions.add(Tacky.Instruction.Return(value))

    fun label(name: String) = instructions.add(Tacky.Instruction.Label(LabelName(name)))

    fun jump(target: String) = instructions.add(Tacky.Instruction.Jump(LabelName(target)))

    fun jumpIfZero(src: Tacky.Value, target: String) = instructions.add(Tacky.Instruction.JumpIfZero(src, LabelName(target)))

    fun jumpIfNotZero(src: Tacky.Value, target: String) = instructions.add(Tacky.Instruction.JumpIfNotZero(src, LabelName(target)))

    fun build() = Tacky.FunctionDefinition(name, instructions)
}

class TackyUnaryOperatorBuilder(val operator: Tacky.UnaryOperator, val src: Tacky.Value) {
    infix fun to(dst: Tacky.Value) = Tacky.Instruction.Unary(operator = operator, src = src, dst = dst)
}

operator fun Tacky.Value.unaryMinus() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.Negate, this)

operator fun Tacky.Value.not() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.LogicalNegate, this)

fun Tacky.Value.complement() = TackyUnaryOperatorBuilder(Tacky.UnaryOperator.Complement, this)

class TackyBinaryOperatorBuilder(val operator: Tacky.BinaryOperator, val left: Tacky.Value, val right: Tacky.Value) {
    infix fun to(dst: Tacky.Value) = Tacky.Instruction.Binary(operator = operator, left = left, right = right, dst = dst)
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
