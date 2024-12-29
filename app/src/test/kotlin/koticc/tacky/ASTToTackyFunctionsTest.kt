package koticc.tacky

import koticc.ast.Type
import koticc.ast.e
import koticc.ast.plus
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.toSymbol
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyFunctionsTest {
    @Test
    fun `should skip function declarations with no body`() {
        val input = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer))
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(defined = false),
            ),
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
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    return_(1.e)
                }
                function("bar", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    return_(2.e)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                "bar" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
            ),
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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    call("foo")
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
            ),
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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    call("foo", 1.e, 2.e + 3.e)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
            ),
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
                function("foo", Type.Function(parameters = listOf(Type.Integer, Type.Integer), returnType = Type.Integer), "a", "b") {
                    return_("a".e + "b".e)
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = listOf(Type.Integer, Type.Integer), returnType = Type.Integer).toSymbol(),
                "a" to Type.Integer.toSymbol(),
                "b" to Type.Integer.toSymbol(),
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

    @Test
    fun `should support non-global functions`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    return_(1.e)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(global = false),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                nonGlobalFunction("foo") {
                    return_(1.t)
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
