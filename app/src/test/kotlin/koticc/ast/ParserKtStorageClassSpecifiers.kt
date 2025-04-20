package koticc.ast

import arrow.core.left
import koticc.VarargArgumentsProvider
import koticc.parseInput
import koticc.token.Location
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class ParserKtStorageClassSpecifiers {
    class VariableStorageClassSpecifiersTestCases :
        VarargArgumentsProvider(
            "int a = 1" to program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign 1.e
                }
            },
            "static int a = 1" to program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a", storageClass = AST.StorageClass.Static) assign 1.e
                }
            },
            "int static a = 1" to program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a", storageClass = AST.StorageClass.Static) assign 1.e
                }
            },
            "int extern a = 1" to program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a", storageClass = AST.StorageClass.Extern) assign 1.e
                }
            },
            "extern int a = 1" to program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a", storageClass = AST.StorageClass.Extern) assign 1.e
                }
            },
        )

    @ParameterizedTest
    @ArgumentsSource(VariableStorageClassSpecifiersTestCases::class)
    fun `should parse storage class specifiers in variable declaration`(
        variableDeclarationInput: String,
        expected: AST.Program,
    ) {
        val input = """
            int main(void) {
                $variableDeclarationInput;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = expected,
            actual = actual,
        )
    }

    class FunctionDeclarationStorageClassSpecifiersTestCases :
        VarargArgumentsProvider(
            "int foo(void)" to program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int))
            },
            "static int foo(void)" to program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
            },
            "int static foo(void)" to program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
            },
            "int extern foo(void)" to program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Extern)
            },
            "extern int foo(void)" to program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Extern)
            },
        )

    @ParameterizedTest
    @ArgumentsSource(FunctionDeclarationStorageClassSpecifiersTestCases::class)
    fun `should parse storage class specifiers in function declaration`(
        functionDeclarationInput: String,
        expected: AST.Program,
    ) {
        val input = """
            $functionDeclarationInput;
        """.trimIndent()

        val actual = parseInput(input)

        assertEqualsIgnoringLocations(
            expected = expected,
            actual = actual,
        )
    }

    class InvalidDeclarationTestCases :
        VarargArgumentsProvider(
            "static a = 1" to "invalid type specifier: ''",
            "extern a = 1" to "invalid type specifier: ''",
            "extern static int a" to "invalid storage class specifier",
            "static a(void)" to "invalid type specifier: ''",
            "extern a(void)" to "invalid type specifier: ''",
        )

    @ParameterizedTest
    @ArgumentsSource(InvalidDeclarationTestCases::class)
    fun `should return error if no type is provided in variable or function declaration`(
        invalidDeclarationInput: String,
        expectedMessage: String,
    ) {
        val input = """
            int main(void) {
                $invalidDeclarationInput;
            }
        """.trimIndent()

        val actual = parseInput(input)

        assertEquals(
            expected = ParserError(
                message = expectedMessage,
                location = Location(2, 5),
            ).left(),
            actual = actual,
        )
    }
}
