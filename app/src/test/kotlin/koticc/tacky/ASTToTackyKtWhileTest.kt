package koticc.tacky

import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.int
import koticc.ast.lt
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ASTToTackyKtWhileTest {
    @Test
    fun `should produce tacky for while`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(1, 2)

                function("main") {
                    assign("a.0", 1.t)

                    label("loop_start.0")
                    assign("tmp.1", "a.0".t lt 10.t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    assign("tmp.2", "a.0".t + 1.t)
                    assign("a.0", "tmp.2".t)
                    label("loop_continue.0")
                    jump("loop_start.0")

                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for while with continue and break`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                        if_(("a.0".e.int() eq 5.e.int()).int()) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(1, 3)

                function("main") {
                    assign("a.0", 1.t)

                    label("loop_start.0")
                    assign("tmp.1", "a.0".t lt 10.t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    assign("tmp.2", "a.0".t + 1.t)
                    assign("a.0", "tmp.2".t)

                    assign("tmp.3", "a.0".t eq 5.t)
                    jumpIfZero("tmp.3".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    jump("loop_start.0")

                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
