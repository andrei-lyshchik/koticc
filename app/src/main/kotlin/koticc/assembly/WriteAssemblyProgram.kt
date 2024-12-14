package koticc.assembly

import arrow.core.Either
import arrow.core.raise.either
import koticc.ast.LabelName
import java.io.IOException
import java.io.Writer

fun writeAssemblyProgram(
    assemblyProgram: Assembly.Program,
    writer: Writer,
): Either<IOException, Unit> = either {
    assemblyProgram.topLevel.forEachIndexed { index, functionDefinition ->
        when (functionDefinition) {
            is Assembly.TopLevel.FunctionDefinition -> {
                writeAssemblyFunctionDefinition(functionDefinition.value, writer).bind()
            }
            is Assembly.TopLevel.StaticVariable -> {
                writeAssemblyStaticVariable(functionDefinition.value, writer).bind()
            }
        }
        if (index != assemblyProgram.topLevel.size - 1) {
            Either.catchOrThrow<IOException, Unit> {
                writer.write("\n\n")
            }.bind()
        }
        writer.flush()
    }
}

private fun writeAssemblyFunctionDefinition(
    functionDefinition: Assembly.FunctionDefinition,
    writer: Writer,
): Either<IOException, Unit> =
    Either.catchOrThrow<IOException, Unit> {
        with(writer) {
            if (functionDefinition.global) {
                write("    .globl _${functionDefinition.name}\n")
            }
            write("_${functionDefinition.name}:\n")
            write("    pushq %rbp\n")
            write("    movq %rsp, %rbp\n")
            functionDefinition.body.forEachIndexed { index, instruction ->
                val indent =
                    when (instruction) {
                        is Assembly.Instruction.Label -> ""
                        else -> "    "
                    }
                write("$indent${instruction.toOutputString()}")
                if (index != functionDefinition.body.size - 1) {
                    write("\n")
                }
            }
        }
    }

private fun writeAssemblyStaticVariable(
    staticVariable: Assembly.StaticVariable,
    writer: Writer,
): Either<IOException, Unit> =
    Either.catchOrThrow<IOException, Unit> {
        with(writer) {
            if (staticVariable.global) {
                write("    .globl _${staticVariable.name}\n")
            }
            if (staticVariable.initialValue == 0) {
                write("    .bss\n")
            } else {
                write("    .data\n")
            }
            write("    .balign 4\n")
            write("_${staticVariable.name}:\n")
            if (staticVariable.initialValue == 0) {
                write("    .zero 4")
            } else {
                write("    .long ${staticVariable.initialValue}")
            }
        }
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
        is Assembly.Instruction.Set -> "set${operator.toOutputString()} ${dst.toOutputString(Size.OneByte)}"
        is Assembly.Instruction.Shift -> "${operator.toOutputString()} %cl, ${dst.toOutputString()}"
        is Assembly.Instruction.Unary -> "${operator.toOutputString()} ${operand.toOutputString()}"
        is Assembly.Instruction.Call -> "call _$name"
        is Assembly.Instruction.DeallocateStack -> "addq \$$size, %rsp"
        is Assembly.Instruction.Push -> "pushq ${operand.toOutputString(size = Size.EightByte)}"
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

private fun Assembly.Operand.toOutputString(size: Size = Size.FourByte): String =
    when (this) {
        is Assembly.Operand.Register -> value.toOutputString(size)
        is Assembly.Operand.Stack -> "$offset(%rbp)"
        is Assembly.Operand.Immediate -> "\$$value"
        is Assembly.Operand.Data -> "_$name(%rip)"
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
    OneByte,
    FourByte,
    EightByte,
}

private fun Assembly.RegisterValue.toOutputString(size: Size): String =
    when (this) {
        Assembly.RegisterValue.Ax -> {
            when (size) {
                Size.OneByte -> "%al"
                Size.FourByte -> "%eax"
                Size.EightByte -> "%rax"
            }
        }
        Assembly.RegisterValue.Cx -> {
            when (size) {
                Size.OneByte -> "%cl"
                Size.FourByte -> "%ecx"
                Size.EightByte -> "%rcx"
            }
        }
        Assembly.RegisterValue.Dx -> {
            when (size) {
                Size.OneByte -> "%dl"
                Size.FourByte -> "%edx"
                Size.EightByte -> "%rdx"
            }
        }

        Assembly.RegisterValue.Di -> {
            when (size) {
                Size.OneByte -> "%dil"
                Size.FourByte -> "%edi"
                Size.EightByte -> "%rdi"
            }
        }
        Assembly.RegisterValue.Si -> {
            when (size) {
                Size.OneByte -> "%sil"
                Size.FourByte -> "%esi"
                Size.EightByte -> "%rsi"
            }
        }
        Assembly.RegisterValue.R8 -> {
            when (size) {
                Size.OneByte -> "%r8b"
                Size.FourByte -> "%r8d"
                Size.EightByte -> "%r8"
            }
        }
        Assembly.RegisterValue.R9 -> {
            when (size) {
                Size.OneByte -> "%r9b"
                Size.FourByte -> "%r9d"
                Size.EightByte -> "%r9"
            }
        }
        Assembly.RegisterValue.R10 -> {
            when (size) {
                Size.OneByte -> "%r10b"
                Size.FourByte -> "%r10d"
                Size.EightByte -> "%r10"
            }
        }
        Assembly.RegisterValue.R11 -> {
            when (size) {
                Size.OneByte -> "%r11b"
                Size.FourByte -> "%r11d"
                Size.EightByte -> "%r11"
            }
        }
    }

private fun LabelName.toLocalOutputString() = "L$value"
