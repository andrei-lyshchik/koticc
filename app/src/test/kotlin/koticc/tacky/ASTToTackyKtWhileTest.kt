package koticc.tacky

import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.lt
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.toSymbol
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ASTToTackyKtWhileTest {
    @Test
    fun `should produce tacky for while`() {
        val input = ValidASTProgram(
            value = program {
                function("main") {
                    int("a.0") assign 1.e
                    while_("a.0".e lt 10.e, loopId = 0) {
                        plusAssign("a.0".e, 1.e)
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameterCount = 0).toSymbol(),
                "a.0" to Type.Integer.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
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
                function("main") {
                    int("a.0") assign 1.e
                    while_("a.0".e lt 10.e, loopId = 0) {
                        plusAssign("a.0".e, 1.e)
                        if_("a.0".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameterCount = 0).toSymbol(),
                "a.0" to Type.Integer.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
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
