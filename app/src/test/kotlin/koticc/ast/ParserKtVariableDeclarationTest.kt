package koticc.ast

import arrow.core.left
import koticc.parseInput
import koticc.token.Location
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import kotlin.test.Test
import kotlin.test.assertEquals

class ParserKtVariableDeclarationTest {
    @ParameterizedTest
    @ValueSource(
        strings = [
            "long",
            "long int",
            "int long",
            "signed long int",
        ],
    )
    fun `should parse long declaration`(typeString: String) {
        val input = """
            int main(void) {
                $typeString a = 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a") assign 1.e
                }
            },
            actual = parseInput(input),
        )
    }

    @ParameterizedTest
    @ValueSource(
        strings = [
            "unsigned long",
            "unsigned long int",
            "long unsigned",
        ],
    )
    fun `should parse unsigned long declaration`(typeString: String) {
        val input = """
            int main(void) {
                $typeString a = 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uLong("a") assign 1.e
                }
            },
            actual = parseInput(input),
        )
    }

    @ParameterizedTest
    @ValueSource(
        strings = [
            "unsigned int",
        ],
    )
    fun `should parse unsigned int declaration`(typeString: String) {
        val input = """
            int main(void) {
                $typeString a = 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uInt("a") assign 1.e
                }
            },
            actual = parseInput(input),
        )
    }

    @ParameterizedTest
    @ValueSource(
        strings = [
            "int",
            "signed int",
            "signed",
        ],
    )
    fun `should parse int declaration`(typeString: String) {
        val input = """
            int main(void) {
                $typeString a = 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign 1.e
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `should parse double declaration`() {
        val input = """
            int main(void) {
                double a = 1;
            }
        """.trimIndent()

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    double("a") assign 1.e
                }
            },
            actual = parseInput(input),
        )
    }

    @Test
    fun `should return error if double is mixed with other types`() {
        val input = """
            int main(void) {
                double int a = 1;
            }
        """.trimIndent()

        assertEquals(
            expected = ParserError("invalid type specifier: 'Double Int'", Location(2, 5)).left(),
            actual = parseInput(input),
        )
    }
}
