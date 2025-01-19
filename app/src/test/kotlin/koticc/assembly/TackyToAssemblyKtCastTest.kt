package koticc.assembly

import koticc.ast.Type
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tackyProgramToAssemblyString
import kotlin.test.Test
import kotlin.test.assertEquals

class TackyToAssemblyKtCastTest {
    @Test
    fun `should generate assembly for sign extend`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(1, 1, type = Type.Long)

            function("foo") {
                signExtend(1.t, "tmp.1")
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
                 movl $1, %r10d
                 movsx %r10d, %r11
                 movq %r11, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }

    @Test
    fun `should generate assembly for truncate`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(1, 1, type = Type.Int)

            function("foo") {
                truncate(1.t, "tmp.1")
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
                movl $1, -4(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }
}
