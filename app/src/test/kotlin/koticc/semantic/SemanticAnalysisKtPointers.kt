package koticc.semantic

import arrow.core.left
import koticc.parseAndAnalyze
import koticc.token.Location
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtPointers {
    @Test
    fun `should fail dereferencing not a pointer`() {
        val input = """
            int main(void) {
                int a = *1;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError(message = "Expected pointer for dereference, got 1", location = Location(line = 2, column = 14)).left(),
            result,
        )
    }

    @Test
    fun `should fail taking address of not lvalue`() {
        val input = """
            int main(void) {
                int a = &1;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError(message = "Can't take address of non lvalue, got 1", location = Location(line = 2, column = 14)).left(),
            result,
        )
    }
}
