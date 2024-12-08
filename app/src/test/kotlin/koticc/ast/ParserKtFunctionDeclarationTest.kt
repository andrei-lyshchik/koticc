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
                function("main")
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
                function("main")
                function("foo")
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
                function("function", "a", "b")
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
                function("main") {
                    func("foo")
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
                function("main") {
                    func("foo") {
                        return_(1.e)
                    }
                }
            },
            actual = actual,
        )
    }
}
