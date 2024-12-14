package koticc.assembly

import koticc.VarargArgumentsProvider
import koticc.assemblyToString
import koticc.ast.LabelName
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.Test
import kotlin.test.assertEquals

class WriteAssemblyProgramKtTest {
    class AssemblyOutputTestCases : VarargArgumentsProvider(
        Assembly.Instruction.AllocateStack(4) to "subq $4, %rsp",
        *listOf(
            Assembly.BinaryOperator.Add to "addl",
            Assembly.BinaryOperator.Sub to "subl",
            Assembly.BinaryOperator.Mul to "imull",
            Assembly.BinaryOperator.And to "andl",
            Assembly.BinaryOperator.Or to "orl",
            Assembly.BinaryOperator.Xor to "xorl",
        ).map { (operator, expectedOperatorString) ->
            Assembly.Instruction.Binary(
                operator = operator,
                src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
            ) to "$expectedOperatorString %eax, %edx"
        }.toTypedArray(),
        Assembly.Instruction.Cdq to "cdq",
        Assembly.Instruction.Cmp(
            src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
        ) to "cmpl %eax, %edx",
        *listOf(
            Assembly.ConditionalOperator.LessThan to "l",
            Assembly.ConditionalOperator.LessThanOrEqual to "le",
            Assembly.ConditionalOperator.GreaterThan to "g",
            Assembly.ConditionalOperator.GreaterThanOrEqual to "ge",
        ).flatMap { (operator, expectedOperatorString) ->
            listOf(
                Assembly.Instruction.ConditionalJump(
                    operator = operator,
                    target = LabelName("label"),
                ) to "j$expectedOperatorString Llabel",
                Assembly.Instruction.Set(
                    operator = operator,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                ) to "set$expectedOperatorString %al",
            )
        }.toTypedArray(),
        Assembly.Instruction.Idiv(
            operand = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
        ) to "idivl %edx",
        Assembly.Instruction.Jump(
            target = LabelName("another_label"),
        ) to "jmp Lanother_label",
        Assembly.Instruction.Label(
            label = LabelName("label"),
        ) to "Llabel:",
        Assembly.Instruction.Mov(
            src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
        ) to "movl %eax, %edx",
        Assembly.Instruction.Shift(
            operator = Assembly.ShiftOperator.Left,
            dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
        ) to "sall %cl, %eax",
        *listOf(
            Assembly.UnaryOperator.Neg to "negl",
            Assembly.UnaryOperator.Not to "notl",
        ).flatMap { (operator, expectedOperatorString) ->
            listOf(
                Assembly.Instruction.Unary(
                    operator = operator,
                    operand = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                ) to "$expectedOperatorString %eax",
            )
        }.toTypedArray(),
        *listOf(
            *listOf(
                Assembly.RegisterValue.Ax to "%eax",
                Assembly.RegisterValue.Cx to "%ecx",
                Assembly.RegisterValue.Dx to "%edx",
                Assembly.RegisterValue.R10 to "%r10d",
                Assembly.RegisterValue.R11 to "%r11d",
            ).map { (registerValue, expectedRegisterString) ->
                Assembly.Operand.Register(registerValue) to expectedRegisterString
            }.toTypedArray(),
            Assembly.Operand.Register(Assembly.RegisterValue.Ax) to "%eax",
            Assembly.Operand.Stack(4) to "4(%rbp)",
            Assembly.Operand.Immediate(42) to "$42",
        ).flatMap { (operand, expectedOperandString) ->
            listOf(
                Assembly.Instruction.Mov(
                    src = operand,
                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                ) to "movl $expectedOperandString, %edx",
            )
        }.toTypedArray(),
        *listOf(
            Assembly.RegisterValue.Ax to "%al",
            Assembly.RegisterValue.Cx to "%cl",
            Assembly.RegisterValue.Dx to "%dl",
            Assembly.RegisterValue.R10 to "%r10b",
            Assembly.RegisterValue.R11 to "%r11b",
        ).map { (registerValue, expectedOutputString) ->
            Assembly.Instruction.Set(
                operator = Assembly.ConditionalOperator.LessThan,
                dst = Assembly.Operand.Register(registerValue),
            ) to "setl $expectedOutputString"
        }.toTypedArray(),
    )

    @ParameterizedTest
    @ArgumentsSource(AssemblyOutputTestCases::class)
    fun `should produce correct output for instructions`(
        instruction: Assembly.Instruction,
        expectedInstructionOutput: String,
    ) {
        val assemblyProgram =
            Assembly.Program(
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body = listOf(instruction),
                        ),
                    ),
                ),
            )
        val output = assemblyToString(assemblyProgram)
        val indent =
            when (instruction) {
                is Assembly.Instruction.Label -> ""
                else -> "    "
            }
        assertEquals(
            """
                .globl _main
            _main:
                pushq %rbp
                movq %rsp, %rbp
            $indent$expectedInstructionOutput
            """.trimIndent(),
            output.trimIndent(),
        )
    }

    @Test
    fun `should product correct output for ret`() {
        val assemblyProgram =
            Assembly.Program(
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body = listOf(Assembly.Instruction.Ret),
                        ),
                    ),
                ),
            )
        val output = assemblyToString(assemblyProgram)
        assertEquals(
            """
                .globl _main
            _main:
                pushq %rbp
                movq %rsp, %rbp
                movq %rbp, %rsp
                popq %rbp
                ret
            """.trimIndent(),
            output.trimIndent(),
        )
    }

    @Test
    fun `test for multiple instructions`() {
        val assembly =
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.Label(LabelName("start")),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Dx),
                                ),
                                Assembly.Instruction.Ret,
                            ),
                        ),
                    ),
                ),
            )

        val output = assemblyToString(assembly)

        assertEquals(
            """
                .globl _main
            _main:
                pushq %rbp
                movq %rsp, %rbp
            Lstart:
                movl $42, %eax
                movl %eax, %edx
                movq %rbp, %rsp
                popq %rbp
                ret
            """.trimIndent(),
            output.trimIndent(),
        )
    }
}
