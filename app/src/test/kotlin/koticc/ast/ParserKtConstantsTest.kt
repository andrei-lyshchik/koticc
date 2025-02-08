package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtConstantsTest {
    @Test
    fun `should parse unsigned long constants`() {
        val input = """
            int main(void) {
                unsigned long ul = 18446744073709551520ul;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uLong("ul") assign 18446744073709551520UL.e
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse double constants`() {
        val input = """
            int main(void) {
                double d = 3.14;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    double("d") assign 3.14.e
                }
            },
            actual = actual,
        )
    }
}
