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
}
