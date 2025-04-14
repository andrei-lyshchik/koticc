package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtDereferenceTest {
    @Test
    fun `should parse dereference operator`() {
        val input = """
            int main(void) {
                int a = *b;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    int("a") assign "b".e.deref()
                }
            },
            actual,
        )
    }

    @Test
    fun `should parse dereference of dereference`() {
        val input = """
            int main(void) {
                int a = **b;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            program {
                main {
                    int("a") assign "b".e.deref().deref()
                }
            },
            actual,
        )
    }
}
