package koticc.tacky

import koticc.ast.Type
import koticc.ast.e
import koticc.ast.int
import koticc.ast.plus
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyFunctionsTest {
    @Test
    fun `should skip function declarations with no body`() {
        val input = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int))
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(defined = false),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable
            },
            actual = actual,
        )
    }

    @Test
    fun `should support multiple functions`() {
        val input = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    return_(1.e)
                }
                function("bar", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    return_(2.e)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "bar" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable

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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    call("foo", type = Type.Int)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(0, 1)

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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    call("foo", 1.e.int(), (2.e.int() + 3.e.int()).int(), type = Type.Int)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(0, 2)

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
                function("foo", Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int), "a", "b") {
                    return_(("a".e.int() + "b".e.int()).int())
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = listOf(Type.Int, Type.Int), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(),
                "b" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(2, 1)

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
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    return_(1.e)
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(global = false),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable

                nonGlobalFunction("foo") {
                    return_(1.t)
                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
