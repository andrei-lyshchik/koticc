package koticc.assembly

import koticc.VarargArgumentsProvider
import koticc.ast.Type
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import koticc.tacky.Tacky
import koticc.tacky.and
import koticc.tacky.complement
import koticc.tacky.div
import koticc.tacky.eq
import koticc.tacky.gt
import koticc.tacky.gte
import koticc.tacky.lt
import koticc.tacky.lte
import koticc.tacky.minus
import koticc.tacky.neq
import koticc.tacky.not
import koticc.tacky.or
import koticc.tacky.plus
import koticc.tacky.rem
import koticc.tacky.shl
import koticc.tacky.shr
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tacky.times
import koticc.tacky.unaryMinus
import koticc.tacky.xor
import koticc.tackyProgramToAssemblyString
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class TackyToAssemblyKtTest {
    @Test
    fun `should generate assembly for single return`() {
        val tacky = tackyProgram {
            function("main") {
                return_(42.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                 .globl _main
             _main:
                 pushq %rbp
                 movq %rsp, %rbp
                 subq $0, %rsp
                 movl $42, %eax
                 movq %rbp, %rsp
                 popq %rbp
                 ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should allocate stack for variables`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("x", 42.t)
                return_("x".t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $42, -4(%rbp)
                     movl -4(%rbp), %eax
                     movq %rbp, %rsp
                     popq %rbp
                     ret
            """.trimIndent(),
            actual = actual,
        )
    }

    class SimpleUnaryTestCases :
        VarargArgumentsProvider(
            Arguments.of(Tacky.UnaryOperator.Negate, "negl"),
            Arguments.of(Tacky.UnaryOperator.Complement, "notl"),
        )

    @ParameterizedTest
    @ArgumentsSource(SimpleUnaryTestCases::class)
    fun `should generate assembly for simple unary operations`(
        tackyOperator: Tacky.UnaryOperator,
        assemblyOperator: String,
    ) {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.UnaryOperator.Negate -> -(42.t)
                        Tacky.UnaryOperator.Complement -> 42.t.complement()
                        Tacky.UnaryOperator.LogicalNegate -> error("Not in this test")
                    },
                )
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $42, -4(%rbp)
                     $assemblyOperator -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate assembly for logical negate`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("x", !42.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $0, %r10d
                    cmpl $42, %r10d
                    movl $0, -4(%rbp)
                    sete -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    class SimpleBinaryTestCases :
        VarargArgumentsProvider(
            Arguments.of(Tacky.BinaryOperator.Add, "addl"),
            Arguments.of(Tacky.BinaryOperator.Subtract, "subl"),
            Arguments.of(Tacky.BinaryOperator.BitwiseAnd, "andl"),
            Arguments.of(Tacky.BinaryOperator.BitwiseOr, "orl"),
            Arguments.of(Tacky.BinaryOperator.BitwiseXor, "xorl"),
        )

    @ParameterizedTest
    @ArgumentsSource(SimpleBinaryTestCases::class)
    fun `should generate assembly for simple binary operations`(
        tackyOperator: Tacky.BinaryOperator,
        assemblyOperator: String,
    ) {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.Add -> 1.t + 2.t
                        Tacky.BinaryOperator.Subtract -> 1.t - 2.t
                        Tacky.BinaryOperator.BitwiseAnd -> 1.t and 2.t
                        Tacky.BinaryOperator.BitwiseOr -> 1.t or 2.t
                        Tacky.BinaryOperator.BitwiseXor -> 1.t xor 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $1, -4(%rbp)
                     $assemblyOperator $2, -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate assembly for multiplication`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("x", 1.t * 2.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $1, -4(%rbp)
                     movl -4(%rbp), %r11d
                     imull $2, %r11d
                     movl %r11d, -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    class IdivInstructionTestCases :
        VarargArgumentsProvider(
            Arguments.of(Tacky.BinaryOperator.Divide, "eax"),
            Arguments.of(Tacky.BinaryOperator.Modulo, "edx"),
        )

    @ParameterizedTest
    @ArgumentsSource(IdivInstructionTestCases::class)
    fun `should generate idiv related insructions`(
        tackyOperator: Tacky.BinaryOperator,
        register: String,
    ) {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
                "y" to Type.Int.toSymbol(),
            )
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.Divide -> 1.t / "y".t
                        Tacky.BinaryOperator.Modulo -> 1.t % "y".t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $1, %eax
                     cdq
                     idivl -4(%rbp)
                     movl %$register, -8(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    class ConditionalTestCases :
        VarargArgumentsProvider(
            Arguments.of(Tacky.BinaryOperator.LessThan, "l"),
            Arguments.of(Tacky.BinaryOperator.LessThanOrEqual, "le"),
            Arguments.of(Tacky.BinaryOperator.GreaterThan, "g"),
            Arguments.of(Tacky.BinaryOperator.GreaterThanOrEqual, "ge"),
            Arguments.of(Tacky.BinaryOperator.Equal, "e"),
            Arguments.of(Tacky.BinaryOperator.NotEqual, "ne"),
        )

    @ParameterizedTest
    @ArgumentsSource(ConditionalTestCases::class)
    fun `should generate conditional instructions`(
        tackyOperator: Tacky.BinaryOperator,
        conditionalOperator: String,
    ) {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.LessThan -> 1.t lt 2.t
                        Tacky.BinaryOperator.LessThanOrEqual -> 1.t lte 2.t
                        Tacky.BinaryOperator.GreaterThan -> 1.t gt 2.t
                        Tacky.BinaryOperator.GreaterThanOrEqual -> 1.t gte 2.t
                        Tacky.BinaryOperator.Equal -> 1.t eq 2.t
                        Tacky.BinaryOperator.NotEqual -> 1.t neq 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $0, -4(%rbp)
                     movl $1, %r10d
                     cmpl $2, %r10d
                     set$conditionalOperator -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    class ShiftTestCases :
        VarargArgumentsProvider(
            Arguments.of(Tacky.BinaryOperator.ShiftLeft, "l"),
            Arguments.of(Tacky.BinaryOperator.ShiftRight, "r"),
        )

    @ParameterizedTest
    @ArgumentsSource(ShiftTestCases::class)
    fun `should generate shift instructions`(
        tackyOperator: Tacky.BinaryOperator,
        shiftOperator: String,
    ) {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.ShiftLeft -> 1.t shl 2.t
                        Tacky.BinaryOperator.ShiftRight -> 1.t shr 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $1, -4(%rbp)
                     movl $2, %ecx
                     sa${shiftOperator}l %cl, -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate label instruction`() {
        val tacky = tackyProgram {
            function("main") {
                label("start")
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $0, %rsp
                Lstart:
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate assembly for jump if not zero`() {
        val tacky = tackyProgram {
            function("main") {
                jumpIfNotZero(42.t, "start")
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $0, %rsp
                     movl $0, %r10d
                     cmpl $42, %r10d
                     jne Lstart
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate assembly for jump if zero`() {
        val tacky = tackyProgram {
            function("main") {
                jumpIfZero(42.t, "start")
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $0, %rsp
                     movl $0, %r10d
                     cmpl $42, %r10d
                     je Lstart
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should generate assembly for jump`() {
        val tacky = tackyProgram {
            function("main") {
                jump("start")
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $0, %rsp
                     jmp Lstart
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should not have mov with two stack offsets`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
                "y" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("y", "x".t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl -4(%rbp), %r10d
                     movl %r10d, -8(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should not have cmp with two stack offsets`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
                "y" to Type.Int.toSymbol(),
                "z" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("z", "x".t lt "y".t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $0, -4(%rbp)
                     movl -8(%rbp), %r10d
                     cmpl %r10d, -12(%rbp)
                     setl -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should not have immediate operand as operand of idiv`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("x", 1.t / 2.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl $1, %eax
                     cdq
                     movl $2, %r10d
                     idivl %r10d
                     movl %eax, -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should not have binary operators with two stack offsets`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "x" to Type.Int.toSymbol(),
                "y" to Type.Int.toSymbol(),
                "z" to Type.Int.toSymbol(),
            )
            function("main") {
                assign("z", "x".t + "y".t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                     .globl _main
                 _main:
                     pushq %rbp
                     movq %rsp, %rbp
                     subq $16, %rsp
                     movl -4(%rbp), %r10d
                     movl %r10d, -8(%rbp)
                     movl -12(%rbp), %r10d
                     addl %r10d, -8(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should copy global attribute for functions`() {
        val tacky = tackyProgram {
            nonGlobalFunction("foo") {
                return_(42.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                _foo:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $0, %rsp
                    movl $42, %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should use movl for comparison result initialization even if comparing quadwords`() {
        val tackyProgram = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(start = 0, count = 1)

            function("main") {
                assign("tmp.0", 1L.t eq 2L.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tackyProgram)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $0, -4(%rbp)
                    movq $1, %r10
                    cmpq $2, %r10
                    sete -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should use movl to initialize logical not result`() {
        val tackyProgram = tackyProgram {
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(start = 0, count = 1)

            function("main") {
                assign("tmp.0", !1L.t)
            }
        }

        val actual = tackyProgramToAssemblyString(tackyProgram)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movq $0, %r10
                    cmpq $1, %r10
                    movl $0, -4(%rbp)
                    sete -4(%rbp)
            """.trimIndent(),
            actual = actual,
        )
    }
}
