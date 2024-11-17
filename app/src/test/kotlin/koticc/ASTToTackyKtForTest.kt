package koticc

import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtForTest {
    @Test
    fun `should produce tacky for for`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    for_(initDecl("i.1", 0.e), "i.1".e lt 10.e, "i.1".e plusAssign 1.e, loopId = 0) {
                        e("a.0".e plusAssign "i.1".e)
                    }
                }
            },
            variableCount = 2,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a.0".t)

                    i(0.t to "i.1".t)
                    label("loop_start.0")

                    i("i.1".t lt 10.t to "tmp.2".t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    i("a.0".t + "i.1".t to "tmp.3".t)
                    i("tmp.3".t to "a.0".t)

                    label("loop_continue.0")
                    i("i.1".t + 1.t to "tmp.4".t)
                    i("tmp.4".t to "i.1".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without initializer`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    for_(null, "a.0".e lt 10.e, "a.0".e plusAssign 1.e, loopId = 0) {
                        e("a.0".e plusAssign 1.e)
                    }
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

                    i("a.0".t lt 10.t to "tmp.1".t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    i("a.0".t + 1.t to "tmp.2".t)
                    i("tmp.2".t to "a.0".t)

                    label("loop_continue.0")
                    i("a.0".t + 1.t to "tmp.3".t)
                    i("tmp.3".t to "a.0".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without condition`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    int("i.1")
                    for_(initExpr("i.1".e assign 0.e), null, "i.1".e plusAssign 1.e, loopId = 0) {
                        e("a.0".e plusAssign "i.1".e)
                    }
                }
            },
            variableCount = 2,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a.0".t)

                    i(0.t to "i.1".t)
                    label("loop_start.0")

                    i("a.0".t + "i.1".t to "tmp.2".t)
                    i("tmp.2".t to "a.0".t)

                    label("loop_continue.0")
                    i("i.1".t + 1.t to "tmp.3".t)
                    i("tmp.3".t to "i.1".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without post`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    int("i.1")
                    for_(initExpr("i.1".e assign 0.e), "i.1".e lt 10.e, null, loopId = 0) {
                        e("a.0".e plusAssign "i.1".e)
                    }
                }
            },
            variableCount = 2,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a.0".t)

                    i(0.t to "i.1".t)
                    label("loop_start.0")

                    i("i.1".t lt 10.t to "tmp.2".t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    i("a.0".t + "i.1".t to "tmp.3".t)
                    i("tmp.3".t to "a.0".t)

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
    fun `should produce tacky for for with break or continue`() {
        val input = ValidASTProgram(
            value = program {
                functionDefinition("main") {
                    int("a.0") assign 1.e
                    for_(initDecl("i.1", 0.e), "i.1".e lt 10.e, "i.1".e plusAssign 1.e, loopId = 0) {
                        e("a.0".e plusAssign "i.1".e)
                        if_("i.1".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            break_(0)
                        }
                    }
                }
            },
            variableCount = 2,
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t to "a.0".t)

                    i(0.t to "i.1".t)
                    label("loop_start.0")

                    i("i.1".t lt 10.t to "tmp.2".t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    i("a.0".t + "i.1".t to "tmp.3".t)
                    i("tmp.3".t to "a.0".t)

                    i("i.1".t eq 5.t to "tmp.4".t)
                    jumpIfZero("tmp.4".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    i("i.1".t + 1.t to "tmp.5".t)
                    i("tmp.5".t to "i.1".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
