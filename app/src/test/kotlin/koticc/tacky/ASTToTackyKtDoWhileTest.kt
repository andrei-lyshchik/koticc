package koticc.tacky

import koticc.ast.e
import koticc.ast.eq
import koticc.ast.lt
import koticc.ast.program
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import koticc.semantic.toIdentifier
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtDoWhileTest {
    @Test
    fun `should produce tacky for do while`() {
        val input = ValidASTProgram(
            value = program {
                function("main") {
                    int("a") assign 1.e
                    do_ {
                        plusAssign("a", 1.e)
                    }.while_("a".e lt 10.e, loopId = 0)
                    return_("a".e)
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "a" to Type.Integer.toIdentifier(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("a", 1.t)

                    label("loop_start.0")
                    assign("tmp.1", "a".t + 1.t)
                    assign("a", "tmp.1".t)

                    label("loop_continue.0")
                    assign("tmp.2", "a".t lt 10.t)
                    jumpIfNotZero("tmp.2".t, "loop_start.0")

                    label("loop_end.0")

                    return_("a".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for do while with break or continue`() {
        val input = ValidASTProgram(
            value = program {
                function("main") {
                    int("a.0") assign 1.e
                    do_ {
                        plusAssign("a.0", 1.e)
                        if_("a.0".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }.while_("a.0".e lt 10.e, loopId = 0)
                    return_("a.0".e)
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "a.0" to Type.Integer.toIdentifier(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("a.0", 1.t)

                    label("loop_start.0")
                    assign("tmp.1", "a.0".t + 1.t)
                    assign("a.0", "tmp.1".t)

                    assign("tmp.2", "a.0".t eq 5.t)
                    jumpIfZero("tmp.2".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    assign("tmp.3", "a.0".t lt 10.t)
                    jumpIfNotZero("tmp.3".t, "loop_start.0")

                    label("loop_end.0")

                    return_("a.0".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
