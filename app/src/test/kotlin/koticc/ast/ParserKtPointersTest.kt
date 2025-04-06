package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtPointersTest {
    @Test
    fun `should parse pointers in variable declarations`() {
        val input = """
            int main(void) {
                int *ptr;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", type = Type.Function(emptyList(), returnType = Type.Int)) {
                    ptr("ptr", to = Type.Int)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse pointers in function declarations`() {
        val input = """
            double *func(int a);
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("func", type = Type.Function(parameters = listOf(Type.Int), returnType = Type.Pointer(Type.Double)), "a")
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse pointers in function parameters`() {
        val input = """
            int func(long *ptr);
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("func", type = Type.Function(parameters = listOf(Type.Pointer(Type.Long)), returnType = Type.Int), "ptr")
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse pointers in top-level declarations`() {
        val input = """
            unsigned long *ptr;
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                ptr("ptr", referenced = Type.ULong)
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse pointers in for declarations`() {
        val input = """
            int main(void) {
                for (int *a = 0;;) {
                }
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(emptyList(), Type.Int)) {
                    for_(
                        initDecl("a", Type.Pointer(Type.Int), 0.e),
                        null,
                        null,
                    ) {}
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse pointers to pointers`() {
        val input = """
            int ***ptr;
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                ptr(
                    "ptr",
                    referenced = Type.Pointer(
                        referenced = Type.Pointer(
                            referenced = Type.Int,
                        ),
                    ),
                )
            },
            actual = actual,
        )
    }
}
