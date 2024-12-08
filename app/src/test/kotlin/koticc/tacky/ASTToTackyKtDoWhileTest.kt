package koticc.tacky

import koticc.ast.e
import koticc.ast.eq
import koticc.ast.lt
import koticc.tacky.eq
import koticc.tacky.lt
import koticc.ast.plusAssign
import koticc.ast.program
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtDoWhileTest {
    @Test
    fun `should produce tacky for do while`() {
        val input = ValidASTProgram(
            value = program {
                func("main") {
                    int("a") assign 1.e
                    do_ {
                        e("a".e plusAssign 1.e)
                    }.while_("a".e lt 10.e, loopId = 0)
                    return_("a".e)
                }
            },
            variableCount = 1,
            types = emptyMap<String, Type>(),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t assignTo "a".t)

                    label("loop_start.0")
                    i("a".t + 1.t assignTo "tmp.1".t)
                    i("tmp.1".t assignTo "a".t)

                    label("loop_continue.0")
                    i("a".t lt 10.t assignTo "tmp.2".t)
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
                func("main") {
                    int("a.0") assign 1.e
                    do_ {
                        e("a.0".e plusAssign 1.e)
                        if_("a.0".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }.while_("a.0".e lt 10.e, loopId = 0)
                    return_("a.0".e)
                }
            },
            variableCount = 1,
            types = emptyMap<String, Type>(),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t assignTo "a.0".t)

                    label("loop_start.0")
                    i("a.0".t + 1.t assignTo "tmp.1".t)
                    i("tmp.1".t assignTo "a.0".t)

                    i("a.0".t eq 5.t assignTo "tmp.2".t)
                    jumpIfZero("tmp.2".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    i("a.0".t lt 10.t assignTo "tmp.3".t)
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
