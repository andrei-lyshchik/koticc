package koticc.ast

import koticc.parseInput
import org.junit.jupiter.api.Test

class ParserKtBreakContinueTest {
    @Test
    fun `should parse break`() {
        val input = """
            int main(void) {
                break;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    break_()
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse continue`() {
        val input = """
            int main(void) {
                continue;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    continue_()
                }
            },
            actual = actual,
        )
    }
}
