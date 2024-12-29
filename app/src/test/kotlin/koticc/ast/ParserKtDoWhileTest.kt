package koticc.ast

import koticc.parseInput
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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign 1.e
                    do_ {
                        plusAssign("a".e, 1.e)
                    }.while_("a".e lt 10.e)
                    return_("a".e)
                }
            },
            actual = actual,
        )
    }
}
