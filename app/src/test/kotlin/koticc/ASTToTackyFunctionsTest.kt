package koticc

import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyFunctionsTest {
    @Test
    fun `should skip function declarations with no body`() {
        val input = ValidASTProgram(
            value = program {
                func("foo")
            },
            variableCount = 0,
            types = emptyMap(),
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
                func("foo") {
                    return_(1.e)
                }
                func("bar") {
                    return_(2.e)
                }
            },
            variableCount = 0,
            types = emptyMap(),
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
                func("main") {
                    e("foo"())
                }
            },
            variableCount = 0,
            types = emptyMap(),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(call("foo") assignTo "tmp.0".t)
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
                func("main") {
                    e("foo"(1.e, 2.e + 3.e))
                }
            },
            variableCount = 0,
            types = emptyMap(),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(2.t + 3.t assignTo "tmp.0".t)
                    i(call("foo", 1.t, "tmp.0".t) assignTo "tmp.1".t)
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
                func("foo", "a", "b") {
                    return_("a".e + "b".e)
                }
            },
            variableCount = 2,
            types = mapOf(
                "foo" to Type.Function(parameterCount = 2),
                "a" to Type.Integer,
                "b" to Type.Integer,
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("foo", "a", "b") {
                    i("a".t + "b".t assignTo "tmp.2".t)
                    return_("tmp.2".t)
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
