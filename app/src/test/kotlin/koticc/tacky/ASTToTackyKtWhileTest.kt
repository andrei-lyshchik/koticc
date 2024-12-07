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
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ASTToTackyKtWhileTest {
    @Test
    fun `should produce tacky for while`() {
        val input = ValidASTProgram(
            value = program {
                func("main") {
                    int("a.0") assign 1.e
                    while_("a.0".e lt 10.e, loopId = 0) {
                        e("a.0".e plusAssign 1.e)
                    }
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
                    i("a.0".t lt 10.t assignTo "tmp.1".t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    i("a.0".t + 1.t assignTo "tmp.2".t)
                    i("tmp.2".t assignTo "a.0".t)
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
                func("main") {
                    int("a.0") assign 1.e
                    while_("a.0".e lt 10.e, loopId = 0) {
                        e("a.0".e plusAssign 1.e)
                        if_("a.0".e eq 5.e) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }
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
                    i("a.0".t lt 10.t assignTo "tmp.1".t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    i("a.0".t + 1.t assignTo "tmp.2".t)
                    i("tmp.2".t assignTo "a.0".t)

                    i("a.0".t eq 5.t assignTo "tmp.3".t)
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
