package koticc.assembly

import koticc.ast.Type
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tackyProgramToAssemblyString
import kotlin.test.Test
import kotlin.test.assertEquals

class TackyToAssemblyKtConstantsTest {
    @Test
    fun `should support unsigned long constants`() {
        val program = tackyProgram {
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ) + tempVariablesSymbolTable(1, 1, type = Type.Long)

            function("foo") {
                assign("tmp.1", 18446744073709551520UL.t)
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
                 movq $-96, -8(%rbp)
            """.trimIndent(),
            actual = result,
        )
    }
}
