package koticc.semantic

import arrow.core.left
import koticc.ast.DUMMY_LOCATION
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.program
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtBreakContinueTest {
    @Test
    fun `should not allow break outside of loop`() {
        val input = program {
            function("main") {
                if_(1.e eq 1.e) {
                    break_()
                }
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "break statement outside of loop",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should not allow continue outside of loop`() {
        val input = program {
            function("main") {
                if_(1.e eq 1.e) {
                    continue_()
                }
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "continue statement outside of loop",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }
}
