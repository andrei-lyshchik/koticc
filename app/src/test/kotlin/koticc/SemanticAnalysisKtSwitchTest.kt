package koticc

import arrow.core.left
import arrow.core.right
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtSwitchTest {
    @Test
    fun `should resolve switch and case ids`() {
        val input = program {
            functionDefinition("main") {
                int("a") assign 1.e
                switch(1.e) {
                    case(1.e) {
                        return_(1.e)
                    }
                    case(2.e) {
                        e("a".e assign 2.e)
                    }
                    return_("a".e)
                    default {
                        return_(3.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    functionDefinition("main") {
                        int("a.0") assign 1.e
                        switch(1.e, switchId = 0, hasDefault = true, caseExpressions = mapOf(1 to 0, 2 to 1)) {
                            case(1.e, caseId = 0, switchId = 0) {
                                return_(1.e)
                            }
                            case(2.e, caseId = 1, switchId = 0) {
                                e("a.0".e assign 2.e)
                            }
                            return_("a.0".e)
                            default(switchId = 0) {
                                return_(3.e)
                            }
                        }
                    }
                },
                variableCount = 1,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `switch without any cases`() {
        val program = program {
            functionDefinition("main") {
                switch(1.e) {
                    return_(1.e)
                }
                return_(0.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    functionDefinition("main") {
                        switch(1.e, switchId = 0, caseExpressions = emptyMap(), hasDefault = false) {
                            return_(1.e)
                        }
                        return_(0.e)
                    }
                },
                variableCount = 0,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for case outside of switch`() {
        val program = program {
            functionDefinition("main") {
                case(1.e) {
                    return_(1.e)
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("case outside of switch", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for duplicate case expressions`() {
        val program = program {
            functionDefinition("main") {
                switch(1.e) {
                    case(1.e.copy(location = Location(0, 0))) {
                        return_(1.e)
                    }
                    case(1.e.copy(location = Location(1, 1))) {
                        return_(2.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("duplicate case expression: 1", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for non constant integer expressions`() {
        val program = program {
            functionDefinition("main") {
                int("a") assign 1.e
                switch(1.e) {
                    case("a".e) {
                        return_(1.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("case expression must be an integer constant", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for default outside of switch`() {
        val program = program {
            functionDefinition("main") {
                default {
                    return_(1.e)
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("default outside of switch", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should correctly assign breaks between loops and switches`() {
        val program = program {
            functionDefinition("main") {
                switch(1.e) {
                    case(1.e) {
                        return_(1.e)
                    }
                    while_(1.e) {
                        break_()
                        switch(2.e) {
                            case(3.e) {
                                return_(1.e)
                            }
                            case(4.e) {
                                break_()
                            }
                            default {
                                return_(10.e)
                            }
                        }
                    }
                    case(2.e) {
                        break_()
                    }
                }
                do_ {
                    break_()
                }.while_(1.e)
                return_(0.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    functionDefinition("main") {
                        switch(1.e, switchId = 0, caseExpressions = mapOf(1 to 0, 2 to 1), hasDefault = false) {
                            case(1.e, caseId = 0, switchId = 0) {
                                return_(1.e)
                            }
                            while_(1.e, loopId = 0) {
                                breakLoop(0)
                                switch(2.e, switchId = 1, caseExpressions = mapOf(3 to 0, 4 to 1), hasDefault = true) {
                                    case(3.e, caseId = 0, switchId = 1) {
                                        return_(1.e)
                                    }
                                    case(4.e, caseId = 1, switchId = 1) {
                                        breakSwitch(1)
                                    }
                                    default(switchId = 1) {
                                        return_(10.e)
                                    }
                                }
                            }
                            case(2.e, caseId = 1, switchId = 0) {
                                breakSwitch(0)
                            }
                        }
                        do_ {
                            breakLoop(1)
                        }.while_(1.e, loopId = 1)
                        return_(0.e)
                    }
                },
                variableCount = 0,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for duplicate default`() {
        val program = program {
            functionDefinition("main") {
                switch(1.e) {
                    default {
                        return_(1.e)
                    }
                    default {
                        return_(2.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("duplicate default case", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }
}
