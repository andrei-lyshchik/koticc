package koticc.tacky

import koticc.ast.Type
import koticc.ast.c
import koticc.ast.e
import koticc.ast.plus
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.toSymbol
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtSwitchTest {
    @Test
    fun `should generate tacky for switch with cases and defaults`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    int("a") assign 1.e
                    switch("a".e + 1.e, switchId = 0, hasDefault = true, caseExpressions = mapOf(1.c to 0, 2.c to 1)) {
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
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameterCount = 0).toSymbol(),
                "a" to Type.Integer.toSymbol(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("a", 1.t)
                    assign("tmp.1", "a".t + 1.t)

                    assign("tmp.2", "tmp.1".t eq 1.t)
                    jumpIfNotZero("tmp.2".t, "switch.0.case.0")

                    assign("tmp.3", "tmp.1".t eq 2.t)
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
                function("main") {
                    int("a") assign 1.e
                    switch("a".e, switchId = 0, hasDefault = false, caseExpressions = mapOf(1.c to 0, 2.c to 1)) {
                        case(1.e, caseId = 0, switchId = 0) {
                            assign("a", 3.e)
                        }
                        breakSwitch(0)
                        case(2.e, caseId = 1, switchId = 0) {
                            assign("a", 2.e)
                        }
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameterCount = 0).toSymbol(),
                "a" to Type.Integer.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    assign("a", 1.t)

                    assign("tmp.1", "a".t eq 1.t)
                    jumpIfNotZero("tmp.1".t, "switch.0.case.0")

                    assign("tmp.2", "a".t eq 2.t)
                    jumpIfNotZero("tmp.2".t, "switch.0.case.1")

                    jump("switch.0.end")

                    label("switch.0.case.0")
                    assign("a", 3.t)
                    jump("switch.0.end")

                    label("switch.0.case.1")
                    assign("a", 2.t)

                    label("switch.0.end")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
