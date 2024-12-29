package koticc.ast

import koticc.parseInput
import org.junit.jupiter.api.Test

class ParserKtFunctionDeclarationTest {
    @Test
    fun `should support function declarations without a body`() {
        val input = """
            int main(void);
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int))
            },
            actual = actual,
        )
    }

    @Test
    fun `should multiple function declarations`() {
        val input = """
            int main(void);
            int foo(void);
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int))
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int))
            },
            actual = actual,
        )
    }

    @Test
    fun `should support function declarations with parameters`() {
        val input = """
            int function(int a, int b);
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("function", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "b")
            },
            actual = actual,
        )
    }

    @Test
    fun `should support function declarations inside a block`() {
        val input = """
            int main(void) {
                int foo(void);
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int))
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should support nested function declarations with body`() {
        val input = """
            int main(void) {
                int foo(void) {
                    return 1;
                }
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        return_(1.e)
                    }
                }
            },
            actual = actual,
        )
    }
}
