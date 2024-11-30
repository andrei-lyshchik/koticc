package koticc

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
                func("main") {
                    int("a") assign 1.e
                    while_("a".e lt 10.e) {
                        e("a".e plusAssign 1.e)
                    }
                    return_("a".e)
                }
            },
            actual = actual,
        )
    }
}
