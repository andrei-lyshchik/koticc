package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtWhileTest {
    @Test
    fun `should parse while`() {
        val input = """
                int main(void) {
                    int a = 1;
                    while (a < 10) {
                        a += 1;
                    }
                    return a;
                }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main") {
                    int("a") assign 1.e
                    while_("a".e lt 10.e) {
                        plusAssign("a".e, 1.e)
                    }
                    return_("a".e)
                }
            },
            actual = actual,
        )
    }
}
