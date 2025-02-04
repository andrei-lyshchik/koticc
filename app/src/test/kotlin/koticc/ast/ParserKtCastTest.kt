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

    @Test
    fun `cast should have higher precedence than assignment`() {
        val input = """
            int main(void) {
                int i = 0;
                i = (long) i = 10;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("i") assign 0.e
                    assign("i".e, cast(Type.Long, "i".e) assign 10.e)
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `cast should have higher precedence than binary operators`() {
        val input = """
            int main(void) {
                int i = 0;
                long j = (long) i + 10;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("i") assign 0.e
                    long("j") assign cast(Type.Long, "i".e) + 10.e
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `cast should have lower precedence than function call`() {
        val input = """
            int main(void) {
                int i = 0;
                long j = (long) foo();
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("i") assign 0.e
                    long("j") assign cast(Type.Long, "foo"())
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `cast should have lower precedence than increment`() {
        val input = """
            int main(void) {
                int i = 0;
                long j = (long) i++;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("i") assign 0.e
                    long("j") assign cast(Type.Long, "i".e.postfixIncrement())
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `should parse chained casts`() {
        val input = """
            int main(void) {
                unsigned long a = (unsigned long) (signed) 1ul;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uLong("a") assign cast(Type.ULong, cast(Type.Int, 1uL.e))
                }
            },
            actual = parseInput(input),
        )
    }
}
