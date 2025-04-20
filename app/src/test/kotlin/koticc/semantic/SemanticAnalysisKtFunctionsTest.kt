package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.VarargArgumentsProvider
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.int
import koticc.ast.invoke
import koticc.ast.long
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
            function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "b") {
                return_("a".e + "b".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a.0", "b.1") {
                        return_(("a.0".e.int() + "b.1".e.int()).int())
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test" to Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "b.1" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if local variable has the same name as a function parameter`() {
        val program = program {
            function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
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

    class ParamsWithSameName :
        VarargArgumentsProvider(
            program {
                function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "a")
            },
            program {
                function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "a") {
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
            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                function("test2", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a")
            function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.0")
                    function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.1")
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `different parameter names don't make function declarations conflicting`() {
        val program = program {
            function("test1", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "c")
            function("test1", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "b", "d")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a.0", "c.1")
                    function("test1", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "b.2", "d.3")
                },
                renamedVariableCount = 4,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int).toSymbol(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should allow nesting function declarations without a body`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                if_(1.e eq 1.e) {
                    function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a")
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        if_((1.e.int() eq 1.e.int()).int()) {
                            function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.0")
                        }
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "test1" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(defined = false),
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingVariableAndFunctionDeclarations :
        VarargArgumentsProvider(
            program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign 1.e
                    function("a", Type.Function(parameters = emptyList(), returnType = Type.Int))
                }
            },
            program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    function("a", Type.Function(parameters = emptyList(), returnType = Type.Int))
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
            function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                return_(1.e + "a".e)
            }
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                call("test", 1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.0") {
                        return_((1.e.int() + "a.0".e.int()).int())
                    }
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        call("test", 1.e.int(), type = Type.Int)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "test" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function is undeclared`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                return_("a".e)
            }
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                return_("a".e)
            }
            function("test2", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test1", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.0") {
                        return_("a.0".e.int())
                    }
                    function("test2", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a.1") {
                        return_("a.1".e.int())
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "test1" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(),
                    "test2" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "a.1" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingFunctionDeclarations :
        VarargArgumentsProvider(
            program {
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                    return_("a".e)
                }
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                    return_("a".e)
                }
            },
            program {
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                    return_("a".e)
                }
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a")
                    return_("test"(1.e))
                }
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                    return_("a".e)
                }
            },
            program {
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a")
                function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "b")
            },
            program {
                function("test", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "a") {
                    return_("a".e)
                }
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    function("test", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "b")
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
            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int))
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
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
            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)

            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                return_("test"())
            }

            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static) {
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)

                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        return_("test"().int())
                    }

                    function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static) {
                        return_(1.e.int())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "test" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(global = false),
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare function both as static and non-static`() {
        val program = program {
            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int))

            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                return_("test"())
            }

            function("test", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static) {
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

            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                // this should conflict with the variable declaration above
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int))
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
            function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
            function("client1", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                call("fun")
            }
            function("client2", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                // block-scope declarations take linkage of visible declaration
                function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int))
            }
            // this should not conflict with the static declaration above due to extern
            function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Extern)
            function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
            function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                return_(1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
                    function("client1", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        call("fun", type = Type.Int)
                    }
                    function("client2", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int))
                    }
                    function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Extern)
                    function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int), storageClass = AST.StorageClass.Static)
                    function("fun", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        return_(1.e.int())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "fun" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(global = false),
                    "client1" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "client2" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `local variables with extern specifier can't have initializers`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("a", Type.Function(parameters = emptyList(), returnType = Type.Int))
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a", storageClass = AST.StorageClass.Extern)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("function 'a' redeclared as a variable", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve function call with casts needed`() {
        val program = program {
            function("foo", Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Int), "a", "b")
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                call("foo", 1.e, 2L.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Int), "a.0", "b.1")
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        call("foo", cast(Type.Long, 1.e.int()).long(), cast(Type.Int, 2L.e.long()).int(), type = Type.Int)
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Int).toSymbol(defined = false),
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve function calls with non-int return type`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                return_(1L.e)
            }
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                call("foo")
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                        return_(1L.e.long())
                    }
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        call("foo", type = Type.Long)
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Long).toSymbol(),
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve return statement with cast needed`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                return_(1.e)
            }
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                return_("foo"())
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                        return_(cast(Type.Long, 1.e.int()).long())
                    }
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        return_(cast(Type.Int, "foo"().long()).int())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Long).toSymbol(),
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should support non-int function parameters`() {
        val program = program {
            function("foo", Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Long), "a", "b") {
                return_("a".e + "b".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Long), "a.0", "b.1") {
                        return_(("a.0".e.long() + cast(Type.Long, "b.1".e.int()).long()).long())
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = listOf(Type.Long, Type.Int), returnType = Type.Long).toSymbol(),
                    "a.0" to Type.Long.toSymbol(),
                    "b.1" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
