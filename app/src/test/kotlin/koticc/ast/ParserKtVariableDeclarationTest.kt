package koticc.ast

import koticc.parseInput
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

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
}
