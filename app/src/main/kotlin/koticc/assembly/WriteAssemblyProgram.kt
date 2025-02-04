package koticc.assembly

import arrow.core.Either
import arrow.core.raise.either
import koticc.ast.LabelName
import koticc.semantic.InitialConstantValue
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
                write("$indent${instruction.toOperatorString()}")
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
            if (staticVariable.initialValue.isZero()) {
                write("    .bss\n")
            } else {
                write("    .data\n")
            }
            write("    .balign ${staticVariable.alignment}\n")
            write("_${staticVariable.name}:\n")
            when (staticVariable.initialValue) {
                is InitialConstantValue.Int -> {
                    if (staticVariable.initialValue.isZero()) {
                        write("    .zero 4")
                    } else {
                        write("    .long ${staticVariable.initialValue.value}")
                    }
                }
                is InitialConstantValue.UInt -> {
                    if (staticVariable.initialValue.isZero()) {
                        write("    .zero 4")
                    } else {
                        write("    .long ${staticVariable.initialValue.value}")
                    }
                }
                is InitialConstantValue.Long -> {
                    if (staticVariable.initialValue.isZero()) {
                        write("    .zero 8")
                    } else {
                        write("    .quad ${staticVariable.initialValue.value}")
                    }
                }
                is InitialConstantValue.ULong -> {
                    if (staticVariable.initialValue.isZero()) {
                        write("    .zero 8")
                    } else {
                        write("    .quad ${staticVariable.initialValue.value}")
                    }
                }
            }
        }
    }

private fun Assembly.Type.instructionSuffix() = when (this) {
    Assembly.Type.LongWord -> "l"
    Assembly.Type.QuadWord -> "q"
}

private fun Assembly.Type.toSize() = when (this) {
    Assembly.Type.LongWord -> Size.FourByte
    Assembly.Type.QuadWord -> Size.EightByte
}

private fun Assembly.Instruction.toOperatorString(): String =
    when (this) {
        is Assembly.Instruction.Binary -> {
            "${toOperatorString()} ${src.toOperatorString(size = type.toSize())}, ${dst.toOperatorString(size = type.toSize())}"
        }
        is Assembly.Instruction.Cdq -> when (type) {
            Assembly.Type.LongWord -> "cdq"
            Assembly.Type.QuadWord -> "cqo"
        }
        is Assembly.Instruction.Cmp -> "cmp${type.instructionSuffix()} ${src.toOperatorString(size = type.toSize())}, ${dst.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.ConditionalJump -> "j${operator.toOperatorString()} ${target.toLocalOutputString()}"
        is Assembly.Instruction.Idiv -> "idiv${type.instructionSuffix()} ${operand.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.Div -> "div${type.instructionSuffix()} ${operand.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.Jump -> "jmp ${target.toLocalOutputString()}"
        is Assembly.Instruction.Label -> "${label.toLocalOutputString()}:"
        is Assembly.Instruction.Mov -> "mov${type.instructionSuffix()} ${src.toOperatorString(size = type.toSize())}, ${dst.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.MovZeroExtend -> error("Bug: mov zero extend should be removed before writing assembly output")
        is Assembly.Instruction.Movsx -> "movslq ${src.toOperatorString(size = Size.FourByte)}, ${dst.toOperatorString(size = Size.EightByte)}"
        Assembly.Instruction.Ret -> "movq %rbp, %rsp\n    popq %rbp\n    ret"
        is Assembly.Instruction.Set -> "set${operator.toOperatorString()} ${dst.toOperatorString(Size.OneByte)}"
        is Assembly.Instruction.Shift -> "${toOperatorString()} %cl, ${dst.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.Unary -> "${toOperatorString()} ${operand.toOperatorString(size = type.toSize())}"
        is Assembly.Instruction.Call -> "call _$name"
        is Assembly.Instruction.Push -> "pushq ${operand.toOperatorString(size = Size.EightByte)}"
    }

private fun Assembly.Instruction.Binary.toOperatorString(): String {
    val operatorString = when (operator) {
        Assembly.BinaryOperator.Add -> "add"
        Assembly.BinaryOperator.Sub -> "sub"
        Assembly.BinaryOperator.Mul -> "imul"
        Assembly.BinaryOperator.And -> "and"
        Assembly.BinaryOperator.Or -> "or"
        Assembly.BinaryOperator.Xor -> "xor"
    }
    return "$operatorString${type.instructionSuffix()}"
}

private fun Assembly.ConditionalOperator.toOperatorString(): String =
    when (this) {
        Assembly.ConditionalOperator.LessThan -> "l"
        Assembly.ConditionalOperator.LessThanOrEqual -> "le"
        Assembly.ConditionalOperator.GreaterThan -> "g"
        Assembly.ConditionalOperator.GreaterThanOrEqual -> "ge"
        Assembly.ConditionalOperator.Equal -> "e"
        Assembly.ConditionalOperator.NotEqual -> "ne"
        Assembly.ConditionalOperator.AboveOrEqual -> "ae"
        Assembly.ConditionalOperator.Above -> "a"
        Assembly.ConditionalOperator.BelowOrEqual -> "be"
        Assembly.ConditionalOperator.Below -> "b"
    }

private fun Assembly.Operand.toOperatorString(size: Size): String =
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

private fun Assembly.Instruction.Shift.toOperatorString(): String {
    val operatorString = when (operator) {
        Assembly.ShiftOperator.Left -> "sal"
        Assembly.ShiftOperator.Right -> "sar"
    }
    return "$operatorString${type.instructionSuffix()}"
}

private fun Assembly.Instruction.Unary.toOperatorString(): String {
    val operatorString = when (operator) {
        Assembly.UnaryOperator.Neg -> "neg"
        Assembly.UnaryOperator.Not -> "not"
    }
    return "$operatorString${type.instructionSuffix()}"
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
        Assembly.RegisterValue.Sp -> {
            when (size) {
                Size.OneByte -> "%spl"
                Size.FourByte -> "%esp"
                Size.EightByte -> "%rsp"
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
