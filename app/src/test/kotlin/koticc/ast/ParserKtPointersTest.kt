package koticc.ast

import arrow.core.left
import koticc.parseInput
import koticc.token.Location
import kotlin.test.Test
import kotlin.test.assertEquals

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
                function(
                    "func",
                    type = Type.Function(parameters = listOf(Type.Int), returnType = Type.Pointer(Type.Double)),
                    "a",
                )
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
                function(
                    "func",
                    type = Type.Function(parameters = listOf(Type.Pointer(Type.Long)), returnType = Type.Int),
                    "ptr",
                )
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

    @Test
    fun `function pointers are not supported`() {
        val input = """
            int (*func)(int a);
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            ParserError("Unsupported declarator type: function pointers are not supported", Location(1, 6)).left(),
            actual,
        )
    }

    @Test
    fun `should parse pointer in casts`() {
        val input = """
            int main(void) {
                int *a = (int *) 0;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    ptr("a", Type.Int) assign cast(Type.Pointer(Type.Int), 0.e)
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse pointer to pointer in casts`() {
        val input = """
            int main(void) {
                int *a = (int ***) 0;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    ptr("a", Type.Int) assign cast(Type.Pointer(Type.Pointer(Type.Pointer(Type.Int))), 0.e)
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse grouped pointers in casts`() {
        val input = """
            int main(void) {
                int *a = (int (*(*))) 0;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    ptr("a", Type.Int) assign cast(Type.Pointer(Type.Pointer(Type.Int)), 0.e)
                }
            },
            actual,
        )
    }
}
