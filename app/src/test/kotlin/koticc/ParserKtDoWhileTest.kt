package koticc

import kotlin.test.Test

class ParserKtDoWhileTest {
    @Test
    fun `should parse do while`() {
        val input = """
            int main(void) {
                int a = 1;
                do {
                    a += 1;
                } while (a < 10);
                return a;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                func("main") {
                    int("a") assign 1.e
                    do_ {
                        e("a".e plusAssign 1.e)
                    }.while_("a".e lt 10.e)
                    return_("a".e)
                }
            },
            actual = actual,
        )
    }
}
