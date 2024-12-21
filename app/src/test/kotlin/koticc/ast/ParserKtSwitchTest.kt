package koticc.ast

import arrow.core.left
import koticc.parseInput
import koticc.token.Location
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ParserKtSwitchTest {
    @Test
    fun `should parse switch`() {
        val input = """
            int main(void) {
                switch (1) {
                    case 1:
                        return 1;
                        break;
                    case 2:
                        return 2;
                    default:
                        return 3;
                }
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main") {
                    switch(1.e) {
                        case(1.e) {
                            return_(1.e)
                        }
                        break_()
                        case(2.e) {
                            return_(2.e)
                        }
                        default {
                            return_(3.e)
                        }
                    }
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should not allow declaration as a case body`() {
        val input = """
            int main(void) {
                switch (1) {
                    case 1:
                        int a = 1;
                        return 1;
x                }
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            expected = ParserError(
                message = "expected factor, got 'int'",
                location = Location(4, 25),
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should not allow declaration as a default body`() {
        val input = """
            int main(void) {
                switch (1) {
                    case 1:
                        return 1;
                    default:
                        int a = 1;
                        return 2;
                }
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            expected = ParserError(
                message = "expected factor, got 'int'",
                location = Location(6, 13),
            ).left(),
            actual = actual,
        )
    }
}
