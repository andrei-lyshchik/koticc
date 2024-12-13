package koticc.ast

import koticc.parseInput
import kotlin.test.Test

class ParserKtFileLevelVariableDeclarationsTest {
    @Test
    fun `should parse file level variable declaration`() {
        val input = """
            int a;
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                int("a")
            },
            actual = actual,
        )
    }

    @Test
    fun `should parse file level variable declaration with initializer`() {
        val input = """
            int a = 1;
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                int("a") assign 1.e
            },
            actual = actual,
        )
    }
}
