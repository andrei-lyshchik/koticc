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
}
