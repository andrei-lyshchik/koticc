package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.int
import koticc.ast.plus
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtFileScopeVariablesTest {
    @Test
    fun `should not rename file scope variables`() {
        val program = program {
            int("a")
            int("b") assign 1.e
        }

        val result = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("a")
                    int("b") assign 1.e.int()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Tentative,
                            global = true,
                        ),
                    ),
                    "b" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(1),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = result,
        )
    }

    @Test
    fun `should be possible to refer to variable from outer scope with extern and do it multiple times`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("foo") assign 1.e
                if_("foo".e) {
                    int("foo", storageClass = AST.StorageClass.Extern)
                    int("foo", storageClass = AST.StorageClass.Extern)
                    return_("foo".e)
                }
            }
            int("foo", storageClass = AST.StorageClass.Extern) assign 2.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("foo.0") assign 1.e.int()
                        if_("foo.0".e.int()) {
                            int("foo", storageClass = AST.StorageClass.Extern)
                            int("foo", storageClass = AST.StorageClass.Extern)
                            return_("foo".e.int())
                        }
                    }
                    int("foo", storageClass = AST.StorageClass.Extern) assign 2.e.int()
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "foo.0" to Type.Int.toSymbol(attributes = VariableAttributes.Local),
                    "foo" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(2),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `file-scope variables can't have non constant initializers`() {
        val program = program {
            int("a") assign (1.e + 2.e)
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "non-constant initializer for file-scope variable 'a'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare a file-scope variable if function with the same name is already declared`() {
        val program = program {
            function("a", Type.Function(parameters = emptyList(), returnType = Type.Int))
            int("a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "function 'a' redeclared as a variable",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't define file scope variables multiple times`() {
        val program = program {
            int("a") assign 1.e
            int("a") assign 2.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "redeclaration of 'a' with a different initializer",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }
}
