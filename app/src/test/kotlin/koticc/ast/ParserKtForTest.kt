package koticc.ast

import koticc.VarargArgumentsProvider
import koticc.parseInput
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsSource

class ParserKtForTest {
    class TestCases :
        VarargArgumentsProvider(
            Arguments.of(
                "int i = 0; i < 10; i += 1",
                initDecl("i", initializer = 0.e),
                "i".e lt 10.e,
                "i".e plusAssign 1.e,
            ),
            Arguments.of(
                "i = 0; i < 10; i += 1",
                initExpr("i".e assign 0.e),
                "i".e lt 10.e,
                "i".e plusAssign 1.e,
            ),
            Arguments.of(
                "; i < 10; i += 1",
                null,
                "i".e lt 10.e,
                "i".e plusAssign 1.e,
            ),
            Arguments.of(
                ";; i += 1",
                null,
                null,
                "i".e plusAssign 1.e,
            ),
            Arguments.of(
                "i = 0; i < 10;",
                initExpr("i".e assign 0.e),
                "i".e lt 10.e,
                null,
            ),
            Arguments.of(
                ";;",
                null,
                null,
                null,
            ),
        )

    @ParameterizedTest
    @ArgumentsSource(TestCases::class)
    fun `should parse for loop`(
        forLoopHeader: String,
        expectedInit: AST.ForInitializer?,
        expectedCondition: AST.Expression?,
        expectedPost: AST.Expression?,
    ) {
        val input = """
            int main(void) {
                int a = 1;
                for ($forLoopHeader) {
                    a += 1;
                }
                return a;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign 1.e
                    for_(
                        expectedInit,
                        expectedCondition,
                        expectedPost,
                    ) {
                        plusAssign("a".e, 1.e)
                    }
                    return_("a".e)
                }
            },
            actual = actual,
        )
    }
}
