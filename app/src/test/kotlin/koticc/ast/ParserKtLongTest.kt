package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtLongTest {
    @Test
    fun `should parse long constants`() {
        val input = """
            int main(void) {
                long a = 1L;
                long b = ${Long.MAX_VALUE};
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a") assign 1L.e
                    long("b") assign Long.MAX_VALUE.e
                }
            },
            actual = parseInput(input),
        )
    }
}
