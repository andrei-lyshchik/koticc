package koticc

import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtDoWhileTest {
    @Test
    fun `should produce tacky for do while`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a") assign 1.e
                    do_ {
                        e("a".e plusAssign 1.e)
                    }.while_("a".e lt 10.e, loopId = 0)
                    return_("a".e)
                }
            },
            variableCount = 1,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a".t)

                    label("loop_start.0")
                    i("a".t + 1.t to "tmp.1".t)
                    i("tmp.1".t to "a".t)

                    label("loop_continue.0")
                    i("a".t lt 10.t to "tmp.2".t)
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
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    do_ {
                        e("a.0".e plusAssign 1.e)
                        if_("a.0".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            break_(0)
                        }
                    }.while_("a.0".e lt 10.e, loopId = 0)
                    return_("a.0".e)
                }
            },
            variableCount = 1,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a.0".t)

                    label("loop_start.0")
                    i("a.0".t + 1.t to "tmp.1".t)
                    i("tmp.1".t to "a.0".t)

                    i("a.0".t eq 5.t to "tmp.2".t)
                    jumpIfZero("tmp.2".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    i("a.0".t lt 10.t to "tmp.3".t)
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
