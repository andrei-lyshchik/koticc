package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtArraysTest {
    @Test
    fun `should parse arrays`() {
        val input = """
            int main(void) {
                int a[3];
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Int, 3)
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse multi-dimensional arrays`() {
        val input = """
            int main(void) {
                long a[3][4];
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Array(Type.Long, 4), 3)
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse compound initializers`() {
        val input = """
            int main(void) {
                int a[3] = {1, 2, 3};
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Int, 3) assign listOf(1.e, 2.e, 3.e)
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse nested compound initializers`() {
        val input = """
            int main(void) {
                int i = 1;
                int a[3][2] = { {1, i}, {i + 1, 4}, {5, 6} };
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    int("i") assign 1.e
                    array("a", Type.Array(Type.Int, 2), 3) assign listOf(
                        AST.VariableInitializer.Compound(
                            listOf(
                                AST.VariableInitializer.Single(1.e),
                                AST.VariableInitializer.Single("i".e),
                            ),
                        ),
                        AST.VariableInitializer.Compound(
                            listOf(
                                AST.VariableInitializer.Single("i".e + 1.e),
                                AST.VariableInitializer.Single(4.e),
                            ),
                        ),
                        AST.VariableInitializer.Compound(
                            listOf(
                                AST.VariableInitializer.Single(5.e),
                                AST.VariableInitializer.Single(6.e),
                            ),
                        ),
                    )
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse array subscript expression`() {
        val input = """
            int main(void) {
                int a[1];
                return a[0];
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Int, 1)
                    return_("a".e[0.e])
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse multi-dimensional subscript expressions`() {
        val input = """
            int main(void) {
                int a[1][2] = { { 1, 2 } };
                int i = 1;
                a[i - 1][i] = 123;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Array(Type.Int, 2), 1) assign listOf(
                        AST.VariableInitializer.Compound(
                            listOf(
                                AST.VariableInitializer.Single(1.e),
                                AST.VariableInitializer.Single(2.e),
                            ),
                        ),
                    )
                    int("i") assign 1.e
                    assign(
                        "a".e["i".e - 1.e]["i".e],
                        123.e,
                    )
                }
            },
            actual,
        )
    }

    @Test
    fun `subscript expression has higher precedence than address of`() {
        val input = """
            int main(void) {
                int a[1];
                int *p = &a[0];
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Int, 1)
                    ptr("p", Type.Int) assign addressOf("a".e[0.e])
                }
            },
            actual,
        )
    }

    @Test
    fun `can have array sizes with non-int literals`() {
        val input = """
            int main(void) {
                int a[10ul];
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("a", Type.Int, 10)
                }
            },
            actual,
        )
    }

    @Test
    fun `should support arrays in casts`() {
        val input = """
            int main(void) {
                int (*arr)[2] = (int (*)[2]) 0;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    ptr("arr", Type.Array(Type.Int, 2)) assign cast(
                        Type.Pointer(Type.Array(Type.Int, 2)),
                        0.e,
                    )
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse casts to arrays`() {
        val input = """
            int main(void) {
                int arr[10] = (int[10]) 0;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    array("arr", Type.Int, 10) assign cast(
                        Type.Array(Type.Int, 10),
                        0.e,
                    )
                }
            },
            actual,
        )
    }
}
