package koticc

import arrow.core.Either
import java.io.IOException
import java.io.Writer

fun writeAssemblyProgram(
    assemblyProgram: Assembly.Program,
    writer: Writer,
): Either<IOException, Unit> =
    Either.catchOrThrow<IOException, Unit> {
        writer.write("    .globl _${assemblyProgram.functionDefinition.name}\n")
        writer.write("_${assemblyProgram.functionDefinition.name}:\n")
        writer.write("    pushq %rbp\n")
        writer.write("    movq %rsp, %rbp\n")
        assemblyProgram.functionDefinition.body.forEach { instruction ->
            val indent =
                when (instruction) {
                    is Assembly.Instruction.Label -> ""
                    else -> "    "
                }
            writer.write("$indent${instruction.toOutputString()}\n")
        }
        writer.flush()
    }

private fun Assembly.Instruction.toOutputString(): String =
    when (this) {
        is Assembly.Instruction.AllocateStack -> "subq \$$size, %rsp"
        is Assembly.Instruction.Binary -> {
            "${operator.toOutputString()} ${src.toOutputString()}, ${dst.toOutputString()}"
        }
        Assembly.Instruction.Cdq -> "cdq"
        is Assembly.Instruction.Cmp -> "cmpl ${src.toOutputString()}, ${dst.toOutputString()}"
        is Assembly.Instruction.ConditionalJump -> "j${operator.toOutputString()} ${target.toLocalOutputString()}"
        is Assembly.Instruction.Idiv -> "idivl ${operand.toOutputString()}"
        is Assembly.Instruction.Jump -> "jmp ${target.toLocalOutputString()}"
        is Assembly.Instruction.Label -> "${label.toLocalOutputString()}:"
        is Assembly.Instruction.Mov -> "movl ${src.toOutputString()}, ${dst.toOutputString()}"
        Assembly.Instruction.Ret -> "movq %rbp, %rsp\n    popq %rbp\n    ret"
        is Assembly.Instruction.Set -> "set${operator.toOutputString()} ${dst.toOutputString(Size.Byte)}"
        is Assembly.Instruction.Shift -> "${operator.toOutputString()} %cl, ${dst.toOutputString()}"
        is Assembly.Instruction.Unary -> "${operator.toOutputString()} ${operand.toOutputString()}"
    }

private fun Assembly.BinaryOperator.toOutputString(): String =
    when (this) {
        Assembly.BinaryOperator.Add -> "addl"
        Assembly.BinaryOperator.Sub -> "subl"
        Assembly.BinaryOperator.Mul -> "imull"
        Assembly.BinaryOperator.And -> "andl"
        Assembly.BinaryOperator.Or -> "orl"
        Assembly.BinaryOperator.Xor -> "xorl"
    }

private fun Assembly.ConditionalOperator.toOutputString(): String =
    when (this) {
        Assembly.ConditionalOperator.LessThan -> "l"
        Assembly.ConditionalOperator.LessThanOrEqual -> "le"
        Assembly.ConditionalOperator.GreaterThan -> "g"
        Assembly.ConditionalOperator.GreaterThanOrEqual -> "ge"
        Assembly.ConditionalOperator.Equal -> "e"
        Assembly.ConditionalOperator.NotEqual -> "ne"
    }

private fun Assembly.Operand.toOutputString(size: Size = Size.LongWord): String =
    when (this) {
        is Assembly.Operand.Register -> value.toOutputString(size)
        is Assembly.Operand.Stack -> "$offset(%rbp)"
        is Assembly.Operand.Immediate -> "\$$value"
        is Assembly.Operand.PseudoIdentifier ->
            error(
                "Bug: pseudo identifiers should be removed " +
                    "before writing assembly output",
            )
    }

private fun Assembly.ShiftOperator.toOutputString(): String =
    when (this) {
        Assembly.ShiftOperator.Left -> "sall"
        Assembly.ShiftOperator.Right -> "sarl"
    }

private fun Assembly.UnaryOperator.toOutputString(): String =
    when (this) {
        Assembly.UnaryOperator.Neg -> "negl"
        Assembly.UnaryOperator.Not -> "notl"
    }

private enum class Size {
    Byte,
    LongWord,
}

private fun Assembly.RegisterValue.toOutputString(size: Size): String =
    when (this) {
        Assembly.RegisterValue.Ax -> {
            when (size) {
                Size.Byte -> "%al"
                Size.LongWord -> "%eax"
            }
        }
        Assembly.RegisterValue.Cx -> {
            when (size) {
                Size.Byte -> "%cl"
                Size.LongWord -> "%ecx"
            }
        }
        Assembly.RegisterValue.Dx -> {
            when (size) {
                Size.Byte -> "%dl"
                Size.LongWord -> "%edx"
            }
        }
        Assembly.RegisterValue.R10 -> {
            when (size) {
                Size.Byte -> "%r10b"
                Size.LongWord -> "%r10d"
            }
        }
        Assembly.RegisterValue.R11 -> {
            when (size) {
                Size.Byte -> "%r11b"
                Size.LongWord -> "%r11d"
            }
        }
    }

private fun LabelName.toLocalOutputString() = "L$value"
