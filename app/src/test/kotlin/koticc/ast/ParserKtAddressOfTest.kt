package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtAddressOfTest {
    @Test
    fun `should parse address of`() {
        val input = """
            int main(void) {
                int a = &b;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    int("a") assign addressOf("b".e)
                }
            },
            actual,
        )
    }
}
