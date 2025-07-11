package koticc.ast

import arrow.core.left
import koticc.VarargArgumentsProvider
import koticc.parseInput
import koticc.token.Location
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.Test
import kotlin.test.assertEquals

class ParserKtFunctionCallTest {
    class FunctionCallArgumentsTestCases :
        VarargArgumentsProvider(
            "" to emptyList<AST.Expression>(),
            "1" to listOf(1.e),
            "1, 2" to listOf(1.e, 2.e),
            "1 + 2, 3 * 4" to listOf(1.e + 2.e, 3.e * 4.e),
        )

    @ParameterizedTest
    @ArgumentsSource(FunctionCallArgumentsTestCases::class)
    fun `should parse function calls`(
        argumentsString: String,
        expectedArguments: List<AST.Expression>,
    ) {
        val input = """
            int main(void) {
                foo($argumentsString);
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    call("foo", *expectedArguments.toTypedArray())
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `function call arguments must be comma separated`() {
        val input = """
            int main(void) {
                foo(1 2);
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            expected = ParserError(
                message = "expected token ,, but got 2",
                location = Location(2, 11),
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `trailing commas are not supported`() {
        val input = """
            int main(void) {
                foo(1, 2,);
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            expected = ParserError(
                message = "expected factor, got ')'",
                location = Location(2, 14),
            ).left(),
            actual = actual,
        )
    }
}
