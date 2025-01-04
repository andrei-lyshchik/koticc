package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtCastTest {
    @Test
    fun `should parse cast from int to long`() {
        val input = """
            int main(void) {
                long a = (long) 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a") assign cast(Type.Long, 1.e)
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `should parse case from long to int`() {
        val input = """
            int main(void) {
                int a = (int) 1L;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign cast(Type.Int, 1L.e)
                }
            },
            actual = parseInput(input),
        )
    }
}
