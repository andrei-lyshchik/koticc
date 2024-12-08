package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.DUMMY_LOCATION
import koticc.VarargArgumentsProvider
import koticc.ast.e
import koticc.tacky.eq
import koticc.ast.AST
import koticc.ast.eq
import koticc.ast.invoke
import koticc.ast.plus
import koticc.tacky.plus
import koticc.ast.program
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtFunctionsTest {
    @Test
    fun `should resolve function params as identifiers`() {
        val program = program {
            func("test", "a", "b") {
                return_("a".e + "b".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("test", "a.0", "b.1") {
                        return_("a.0".e + "b.1".e)
                    }
                },
                variableCount = 2,
                types = mapOf(
                    "test" to Type.Function(parameterCount = 2),
                    "a.0" to Type.Integer,
                    "b.1" to Type.Integer,
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if local variable has the same name as a function parameter`() {
        val program = program {
            func("test", "a") {
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
            func("test", "a", "a")
        },
        program {
            func("test", "a", "a") {
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
            func("test") {
                func("test2") {
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
            func("test1", "a")
            func("test1", "a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("test1", "a.0")
                    func("test1", "a.1")
                },
                variableCount = 2,
                types = mapOf(
                    "test1" to Type.Function(parameterCount = 1),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `different parameter names don't make function declarations conflicting`() {
        val program = program {
            func("test1", "a", "c")
            func("test1", "b", "d")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("test1", "a.0", "c.1")
                    func("test1", "b.2", "d.3")
                },
                variableCount = 4,
                types = mapOf(
                    "test1" to Type.Function(parameterCount = 2),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should allow nesting function declarations without a body`() {
        val program = program {
            func("main") {
                if_(1.e eq 1.e) {
                    func("test1", "a")
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        if_(1.e eq 1.e) {
                            func("test1", "a.0")
                        }
                    }
                },
                variableCount = 1,
                types = mapOf(
                    "main" to Type.Function(parameterCount = 0),
                    "test1" to Type.Function(parameterCount = 1),
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingVariableAndFunctionDeclarations : VarargArgumentsProvider(
        program {
            func("main") {
                int("a") assign 1.e
                func("a")
            }
        },
        program {
            func("main") {
                func("a")
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
            func("test", "a") {
                return_(1.e + "a".e)
            }
            func("main") {
                e("test"(1.e))
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("test", "a.0") {
                        return_(1.e + "a.0".e)
                    }
                    func("main") {
                        e("test"(1.e))
                    }
                },
                variableCount = 1,
                types = mapOf(
                    "main" to Type.Function(parameterCount = 0),
                    "test" to Type.Function(parameterCount = 1),
                    "a.0" to Type.Integer,
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function is undeclared`() {
        val program = program {
            func("main") {
                e("test"())
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
            func("main") {
                int("a") assign 1.e
                e("a"())
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
            func("test", "a") {
                return_("a".e)
            }
            func("main") {
                e("test"(1.e, 2.e))
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
            func("test1", "a") {
                return_("a".e)
            }
            func("test2", "a") {
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("test1", "a.0") {
                        return_("a.0".e)
                    }
                    func("test2", "a.1") {
                        return_("a.1".e)
                    }
                },
                variableCount = 2,
                types = mapOf(
                    "test1" to Type.Function(parameterCount = 1),
                    "test2" to Type.Function(parameterCount = 1),
                    "a.0" to Type.Integer,
                    "a.1" to Type.Integer,
                ),
            ).right(),
            actual = actual,
        )
    }

    class ConflictingFunctionDeclarations : VarargArgumentsProvider(
        program {
            func("test", "a") {
                return_("a".e)
            }
            func("test", "a") {
                return_("a".e)
            }
        },
        program {
            func("test", "a") {
                return_("a".e)
            }
            func("main") {
                func("test", "a")
                return_("test"(1.e))
            }
            func("test", "a") {
                return_("a".e)
            }
        },
        program {
            func("test", "a")
            func("test", "a", "b")
        },
        program {
            func("test", "a") {
                return_("a".e)
            }
            func("main") {
                func("test", "a", "b")
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
            func("test")
            func("main") {
                int("a") assign (1.e + "test".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("'test' is not a variable", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }
}
