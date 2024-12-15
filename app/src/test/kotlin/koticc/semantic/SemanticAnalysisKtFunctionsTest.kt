package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.VarargArgumentsProvider
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.invoke
import koticc.ast.plus
import koticc.ast.program
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtFunctionsTest {
    @Test
    fun `should resolve function params as identifiers`() {
        val program = program {
            function("test", "a", "b") {
                return_("a".e + "b".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", "a.0", "b.1") {
                        return_("a.0".e + "b.1".e)
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test" to Type.Function(parameterCount = 2).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "b.1" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if local variable has the same name as a function parameter`() {
        val program = program {
            function("test", "a") {
                int("a") assign 1.e
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'a' already declared at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    class ParamsWithSameName : VarargArgumentsProvider(
        program {
            function("test", "a", "a")
        },
        program {
            function("test", "a", "a") {
                return_("a".e)
            }
        },
    )

    @ParameterizedTest
    @ArgumentsSource(ParamsWithSameName::class)
    fun `should return error if function parameters has the same name`(
        program: AST.Program,
    ) {
        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'a' already declared at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if nested function has a body`() {
        val program = program {
            function("test") {
                function("test2") {
                    return_(2.e)
                }
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("nested function declaration 'test2' with body", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should allow multiple non-conflicting function declarations without body`() {
        val program = program {
            function("test1", "a")
            function("test1", "a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", "a.0")
                    function("test1", "a.1")
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameterCount = 1).toIdentifier(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `different parameter names don't make function declarations conflicting`() {
        val program = program {
            function("test1", "a", "c")
            function("test1", "b", "d")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", "a.0", "c.1")
                    function("test1", "b.2", "d.3")
                },
                renamedVariableCount = 4,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameterCount = 2).toIdentifier(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should allow nesting function declarations without a body`() {
        val program = program {
            function("main") {
                if_(1.e eq 1.e) {
                    function("test1", "a")
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main") {
                        if_(1.e eq 1.e) {
                            function("test1", "a.0")
                        }
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "test1" to Type.Function(parameterCount = 1).toIdentifier(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingVariableAndFunctionDeclarations : VarargArgumentsProvider(
        program {
            function("main") {
                int("a") assign 1.e
                function("a")
            }
        },
        program {
            function("main") {
                function("a")
                int("a") assign 1.e
            }
        },
    )

    @ParameterizedTest
    @ArgumentsSource(ConflictingVariableAndFunctionDeclarations::class)
    fun `should return error if function is declared in the same scope as variable`(
        program: AST.Program,
    ) {
        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'a' already declared at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `valid function call`() {
        val program = program {
            function("test", "a") {
                return_(1.e + "a".e)
            }
            function("main") {
                call("test", 1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", "a.0") {
                        return_(1.e + "a.0".e)
                    }
                    function("main") {
                        call("test", 1.e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "test" to Type.Function(parameterCount = 1).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function is undeclared`() {
        val program = program {
            function("main") {
                call("test")
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("undeclared function 'test'", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if variable is called as function`() {
        val program = program {
            function("main") {
                int("a") assign 1.e
                call("a")
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'a' is not a function", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function is called with a different number of arguments`() {
        val program = program {
            function("test", "a") {
                return_("a".e)
            }
            function("main") {
                call("test", 1.e, 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("function 'test' expects 1 arguments, but 2 were provided", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should treat different function parameters as in different scopes`() {
        val program = program {
            function("test1", "a") {
                return_("a".e)
            }
            function("test2", "a") {
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", "a.0") {
                        return_("a.0".e)
                    }
                    function("test2", "a.1") {
                        return_("a.1".e)
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameterCount = 1).toIdentifier(),
                    "test2" to Type.Function(parameterCount = 1).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "a.1" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingFunctionDeclarations : VarargArgumentsProvider(
        program {
            function("test", "a") {
                return_("a".e)
            }
            function("test", "a") {
                return_("a".e)
            }
        },
        program {
            function("test", "a") {
                return_("a".e)
            }
            function("main") {
                function("test", "a")
                return_("test"(1.e))
            }
            function("test", "a") {
                return_("a".e)
            }
        },
        program {
            function("test", "a")
            function("test", "a", "b")
        },
        program {
            function("test", "a") {
                return_("a".e)
            }
            function("main") {
                function("test", "a", "b")
                return_(1.e)
            }
        },
    )

    @ParameterizedTest
    @ArgumentsSource(ConflictingFunctionDeclarations::class)
    fun `should return error if function declaration are conflicting`(
        program: AST.Program,
    ) {
        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("conflicting declaration of 'test' at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function is used as a variable`() {
        val program = program {
            function("test")
            function("main") {
                int("a") assign (1.e + "test".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'test' is not a variable", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `nested function declarations can't have static storage`() {
        val program = program {
            function("main") {
                function("test", storageClass = AST.StorageClass.Static)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("nested function declaration 'test' with static storage class", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `static functions are not global`() {
        val program = program {
            function("test", storageClass = AST.StorageClass.Static)

            function("main") {
                return_("test"())
            }

            function("test", storageClass = AST.StorageClass.Static) {
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", storageClass = AST.StorageClass.Static)

                    function("main") {
                        return_("test"())
                    }

                    function("test", storageClass = AST.StorageClass.Static) {
                        return_(1.e)
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "test" to Type.Function(parameterCount = 0).toIdentifier(global = false),
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare function both as static and non-static`() {
        val program = program {
            function("test")

            function("main") {
                return_("test"())
            }

            function("test", storageClass = AST.StorageClass.Static) {
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("static function 'test' declaration follows non-static at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't redeclare function if variable with linkage was already declared with the same name`() {
        val program = program {
            int("foo") assign 10.e

            function("main") {
                // this should conflict with the variable declaration above
                function("foo")
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("variable 'foo' redeclared as a function", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `can declare static functions multiple times`() {
        val program = program {
            function("fun", storageClass = AST.StorageClass.Static)
            function("client1") {
                "fun"()
            }
            function("client2") {
                // block-scope declarations take linkage of visible declaration
                function("fun")
            }
            // this should not conflict with the static declaration above due to extern
            function("fun", storageClass = AST.StorageClass.Extern)
            function("fun", storageClass = AST.StorageClass.Static)
            function("fun") {
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("fun", storageClass = AST.StorageClass.Static)
                    function("client1") {
                        "fun"()
                    }
                    function("client2") {
                        function("fun")
                    }
                    function("fun", storageClass = AST.StorageClass.Extern)
                    function("fun", storageClass = AST.StorageClass.Static)
                    function("fun") {
                        return_(1.e)
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "fun" to Type.Function(parameterCount = 0).toIdentifier(global = false),
                    "client1" to Type.Function(parameterCount = 0).toIdentifier(),
                    "client2" to Type.Function(parameterCount = 0).toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `local variables with extern specifier can't have initializers`() {
        val program = program {
            function("main") {
                int("a", storageClass = AST.StorageClass.Extern) assign 1.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("extern local variable 'a' cannot have an initializer", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare local variable with extern if there's already function with the same name`() {
        val program = program {
            function("a")
            function("main") {
                int("a", storageClass = AST.StorageClass.Extern)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("function 'a' redeclared as a variable", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }
}
