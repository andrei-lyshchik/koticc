package koticc.tacky

import koticc.ast.e
import koticc.ast.plus
import koticc.ast.program
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import koticc.semantic.toIdentifier
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyFunctionsTest {
    @Test
    fun `should skip function declarations with no body`() {
        val input = ValidASTProgram(
            value = program {
                function("foo")
            },
            renamedVariableCount = 0,
            typedIdentifiers = emptyMap(),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram { },
            actual = actual,
        )
    }

    @Test
    fun `should support multiple functions`() {
        val input = ValidASTProgram(
            value = program {
                function("foo") {
                    return_(1.e)
                }
                function("bar") {
                    return_(2.e)
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = emptyMap(),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("foo") {
                    return_(1.t)
                    return_(0.t)
                }
                function("bar") {
                    return_(2.t)
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for function call without arguments`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    call("foo")
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = emptyMap(),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("tmp.0", call("foo"))
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for function call with arguments`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    call("foo", 1.e, 2.e + 3.e)
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = emptyMap(),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("tmp.0", 2.t + 3.t)
                    assign("tmp.1", call("foo", 1.t, "tmp.0".t))
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for function with parameters`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", "a", "b") {
                    return_("a".e + "b".e)
                }
            },
            renamedVariableCount = 2,
            typedIdentifiers = mapOf(
                "foo" to Type.Function(parameterCount = 2).toIdentifier(),
                "a" to Type.Integer.toIdentifier(),
                "b" to Type.Integer.toIdentifier(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("foo", "a", "b") {
                    assign("tmp.2", "a".t + "b".t)
                    return_("tmp.2".t)
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
