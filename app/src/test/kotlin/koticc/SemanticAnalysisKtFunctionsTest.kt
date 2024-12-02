package koticc

import arrow.core.left
import arrow.core.right
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
            expected = SemanticAnalysisError("variable 'a' already declared at line 0, column 0", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if function parameters has the same name`() {
        val program = program {
            func("test", "a", "a") {
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("variable 'a' already declared at line 0, column 0", DUMMY_LOCATION).left(),
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
                    func("test1", "a")
                    func("test1", "a")
                },
                variableCount = 0,
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
                            func("test1", "a")
                        }
                    }
                },
                variableCount = 0,
            ).right(),
            actual = actual,
        )
    }
}
