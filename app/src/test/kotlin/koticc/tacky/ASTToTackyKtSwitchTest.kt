package koticc.tacky

import koticc.ast.assign
import koticc.ast.e
import koticc.ast.plus
import koticc.tacky.plus
import koticc.ast.program
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtSwitchTest {
    @Test
    fun `should generate tacky for switch with cases and defaults`() {
        val program = ValidASTProgram(
            value = program {
                func("main") {
                    int("a") assign 1.e
                    switch("a".e + 1.e, switchId = 0, hasDefault = true, caseExpressions = mapOf(1 to 0, 2 to 1)) {
                        case(1.e, caseId = 0, switchId = 0) {
                            return_(1.e)
                        }
                        case(2.e, caseId = 1, switchId = 0) {
                            return_(2.e)
                        }
                        default(switchId = 0) {
                            return_(3.e)
                        }
                    }
                }
            },
            variableCount = 1,
            types = emptyMap<String, Type>(),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t assignTo "a".t)
                    i("a".t + 1.t assignTo "tmp.1".t)

                    i("tmp.1".t eq 1.t assignTo "tmp.2".t)
                    jumpIfNotZero("tmp.2".t, "switch.0.case.0")

                    i("tmp.1".t eq 2.t assignTo "tmp.3".t)
                    jumpIfNotZero("tmp.3".t, "switch.0.case.1")

                    jump("switch.0.default")

                    label("switch.0.case.0")
                    return_(1.t)

                    label("switch.0.case.1")
                    return_(2.t)

                    label("switch.0.default")
                    return_(3.t)

                    label("switch.0.end")

                    return_(0.t)
                }
            },
            actual = tacky,
        )
    }

    @Test
    fun `should generate tacky for switch with breaks and without default`() {
        val program = ValidASTProgram(
            value = program {
                func("main") {
                    int("a") assign 1.e
                    switch("a".e, switchId = 0, hasDefault = false, caseExpressions = mapOf(1 to 0, 2 to 1)) {
                        case(1.e, caseId = 0, switchId = 0) {
                            e("a".e assign 3.e)
                        }
                        breakSwitch(0)
                        case(2.e, caseId = 1, switchId = 0) {
                            e("a".e assign 2.e)
                        }
                    }
                }
            },
            variableCount = 1,
            types = emptyMap<String, Type>(),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    i(1.t assignTo "a".t)

                    i("a".t eq 1.t assignTo "tmp.1".t)
                    jumpIfNotZero("tmp.1".t, "switch.0.case.0")

                    i("a".t eq 2.t assignTo "tmp.2".t)
                    jumpIfNotZero("tmp.2".t, "switch.0.case.1")

                    jump("switch.0.end")

                    label("switch.0.case.0")
                    i(3.t assignTo "a".t)
                    jump("switch.0.end")

                    label("switch.0.case.1")
                    i(2.t assignTo "a".t)

                    label("switch.0.end")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
