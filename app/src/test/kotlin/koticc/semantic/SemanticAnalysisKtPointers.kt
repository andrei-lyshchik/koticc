package koticc.semantic

import arrow.core.left
import koticc.ast.Type
import koticc.ast.assertEqualsIgnoringLocations
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.program
import koticc.parseAndAnalyze
import koticc.token.Location
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtPointers {
    @Test
    fun `should fail dereferencing not a pointer`() {
        val input = """
            int main(void) {
                int a = *1;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError(message = "Expected pointer for dereference, got 1", location = Location(line = 2, column = 14)).left(),
            result,
        )
    }

    @Test
    fun `should fail taking address of not lvalue`() {
        val input = """
            int main(void) {
                int a = &1;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError(message = "Can't take address of non lvalue, got 1", location = Location(line = 2, column = 14)).left(),
            result,
        )
    }

    @ParameterizedTest
    @ValueSource(strings = ["/", "*", "%"])
    fun `can't divide or multiply pointers`(operator: String) {
        val input = """
            int main(void) {
                int *a = 0;
                return a $operator 3;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("invalid pointer type for '$operator'", Location(3, 12)).left(),
            result,
        )
    }

    @ParameterizedTest
    @ValueSource(strings = ["-", "~"])
    fun `can't negate or complement pointers`(operator: String) {
        val input = """
            int main(void) {
                int *a = 0;
                $operator a;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("invalid operand type for '$operator': int *", Location(3, 5)).left(),
            result,
        )
    }

    @Test
    fun `can't compare pointers and non-zero constants`() {
        val input = """
            int main(void) {
                int *a = 0;
                return a == 123;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("incompatible types: int *, int", Location(3, 12)).left(),
            result,
        )
    }

    @Test
    fun `should cast null pointer constant to pointer type`() {
        val input = """
            int main(void) {
                int *a = 0;
                return a == 0;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEqualsIgnoringLocations(
            ValidASTProgram(
                value = program {
                    main {
                        ptr("a.0", Type.Int) assign cast(Type.Pointer(Type.Int), 0.e.ofType(Type.Int)).ofType(Type.Pointer(Type.Int))
                        return_("a.0".e.ofType(Type.Pointer(Type.Int)).eq(cast(Type.Pointer(Type.Int), 0.e.ofType(Type.Int)).ofType(Type.Pointer(Type.Int))).ofType(Type.Int))
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Pointer(Type.Int).toSymbol(),
                ),
            ),
            result,
        )
    }

    @Test
    fun `can't initialize pointer with non-zero constant`() {
        val input = """
            int main(void) {
                int *a = 123;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Cannot convert 123 to type int *", Location(2, 14)).left(),
            result,
        )
    }

    @Test
    fun `can't assign pointer with non-zero constant`() {
        val input = """
            int main(void) {
                int *a;
                a = 123;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Cannot convert 123 to type int *", Location(3, 9)).left(),
            result,
        )
    }

    @Test
    fun `can't return non-zero constant in function which returns pointer`() {
        val input = """
            int *func(void) {
                return 123;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Cannot convert 123 to type int *", Location(2, 12)).left(),
            result,
        )
    }

    @Test
    fun `should insert casts for null pointer constant casts to pointers`() {
        val input = """
            int* func(void) {
                return 0;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEqualsIgnoringLocations(
            ValidASTProgram(
                value = program {
                    function("func", Type.Function(emptyList(), Type.Pointer(Type.Int))) {
                        return_(cast(Type.Pointer(Type.Int), 0.e.ofType(Type.Int)).ofType(Type.Pointer(Type.Int)))
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "func" to Symbol.Function(
                        type = Type.Function(emptyList(), Type.Pointer(Type.Int)),
                        defined = true,
                        global = true,
                    ),
                ),
            ),
            result,
        )
    }

    @Test
    fun `can't assign static pointer variables with non-zero constants`() {
        val input = """
            int main(void) {
                static int *a = 123;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("invalid initializer '123' for variable 'a'", Location(2, 5)).left(),
            result,
        )
    }

    @Test
    fun `can't assign file scope pointer variables with non-zero constants`() {
        val input = """
            int *a = 123;
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("invalid initializer '123' for variable 'a'", Location(1, 1)).left(),
            result,
        )
    }

    @Test
    fun `can't cast double to pointer`() {
        val input = """
            int main(void) {
                double d = 0.0;
                int *a = (int *) d;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("can't cast expression of type double to int *", Location(3, 14)).left(),
            result,
        )
    }

    @Test
    fun `can't cast pointer to double`() {
        val input = """
            int main(void) {
                int *a = 0;
                double d = (double) a;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("can't cast expression of type int * to double", Location(3, 16)).left(),
            result,
        )
    }

    @Test
    fun `can't pass int as pointer function parameter`() {
        val input = """
            int abc(int *a);
            int main(void) {
                int a = 1;
                abc(a);
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Cannot convert a.1 to type int *", Location(4, 9)).left(),
            result,
        )
    }

    @Test
    fun `can't switch on a pointer`() {
        val input = """
            int main(void) {
                int *p = 0;
                switch (p) {
                default:
                    return 1;
                }
                return 0;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("invalid type int * for switch controlling expression", Location(3, 13)).left(),
            result,
        )
    }

    @Test
    fun `can't add two pointers`() {
        val input = """
            int main(void) {
                int v = 1;
                int *a = &v;
                int *b = &v;
                a + b;
                return 0;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Can't add two pointers", Location(5, 5)).left(),
            result,
        )
    }

    @Test
    fun `can't compare pointer to zero with less or more than`() {
        val input = """
            int main(void) {
                int v = 0;
                int *p = &v;
                p > 0;
                return 0;
            }
        """.trimIndent()

        val result = parseAndAnalyze(input)

        assertEquals(
            SemanticAnalysisError("Can't compare a pointer and a number", Location(4, 5)).left(),
            result,
        )
    }
}
