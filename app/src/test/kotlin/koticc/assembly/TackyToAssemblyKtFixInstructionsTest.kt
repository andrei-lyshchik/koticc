package koticc.assembly

import koticc.ast.Type
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import koticc.tacky.call
import koticc.tacky.eq
import koticc.tacky.minus
import koticc.tacky.plus
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tacky.times
import koticc.tackyProgramToAssemblyString
import kotlin.test.Test
import kotlin.test.assertEquals

class TackyToAssemblyKtFixInstructionsTest {
    @Test
    fun `should copy large immediate sources of add instruction to r10`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("foo") {
                assign("tmp.0", (1L.t + 9223372036854775806L.t))
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                .globl _foo
            _foo:
                pushq %rbp
                movq %rsp, %rbp
                subq $16, %rsp
                movq $1, -8(%rbp)
                movq $9223372036854775806, %r10
                addq %r10, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should copy large immediate sources of sub instruction to r10`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("foo") {
                assign("tmp.0", (1L.t - 9223372036854775806L.t))
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                .globl _foo
            _foo:
                pushq %rbp
                movq %rsp, %rbp
                subq $16, %rsp
                movq $1, -8(%rbp)
                movq $9223372036854775806, %r10
                subq %r10, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should copy large immediate sources of mul instruction to r10`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("foo") {
                assign("tmp.0", (1L.t * 9223372036854775806L.t))
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                .globl _foo
            _foo:
                pushq %rbp
                movq %rsp, %rbp
                subq $16, %rsp
                movq $1, -8(%rbp)
                movq $9223372036854775806, %r10
                movq -8(%rbp), %r11
                imulq %r10, %r11
                movq %r11, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should copy large immediate sources of cmp instructions to r11`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("foo") {
                assign("tmp.0", (1L.t eq 9223372036854775806L.t))
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                .globl _foo
            _foo:
                pushq %rbp
                movq %rsp, %rbp
                subq $16, %rsp
                movq $0, -8(%rbp)
                movq $9223372036854775806, %r11
                movq $1, %r10
                cmpq %r11, %r10
                sete -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should replace mov of large immediate values first to r10 and then to memory`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("foo") {
                assign("tmp.0", 9223372036854775806L.t)
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                .globl _foo
            _foo:
                pushq %rbp
                movq %rsp, %rbp
                subq $16, %rsp
                movq $9223372036854775806, %r10
                movq %r10, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should replace large immediate sources of push with r10`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "caller" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                // first six arguments are passed in registers, so only the seventh argument is pushed
                "callee" to Type.Function(parameters = listOf(Type.Long, Type.Long, Type.Long, Type.Long, Type.Long, Type.Long, Type.Long), returnType = Type.Long).toSymbol(),
            ) + tempVariablesSymbolTable(0, 1, type = Type.Long)

            function("caller") {
                assign("tmp.0", call("callee", 1L.t, 1L.t, 1L.t, 1L.t, 1L.t, 1L.t, 9223372036854775806L.t))
            }
        }

        val result = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _caller
                _caller:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    subq $8, %rsp
                    movq $1, %rdi
                    movq $1, %rsi
                    movq $1, %rdx
                    movq $1, %rcx
                    movq $1, %r8
                    movq $1, %r9
                    movq $9223372036854775806, %r10
                    pushq %r10
                    call _callee
                    addq $16, %rsp
                    movq %rax, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }
}
