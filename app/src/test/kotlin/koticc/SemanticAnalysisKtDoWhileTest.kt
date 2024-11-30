package koticc

import arrow.core.right
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtDoWhileTest {
    @Test
    fun `should assign labels to do while`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                do_ {
                    e("a".e plusAssign 1.e)
                }.while_("a".e lt 10.e)
                do_ {
                    e("a".e plusMultiply 2.e)
                }.while_("a".e lt 40.e)
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        do_ {
                            e("a.0".e plusAssign 1.e)
                        }.while_("a.0".e lt 10.e, loopId = 0)
                        do_ {
                            e("a.0".e plusMultiply 2.e)
                        }.while_("a.0".e lt 40.e, loopId = 1)
                        return_("a.0".e)
                    }
                },
                variableCount = 1,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue inside do while`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                do_ {
                    e("a".e plusAssign 1.e)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                    "a".e plusAssign 1.e
                }.while_("a".e lt 10.e)
                do_ {
                    e("a".e plusAssign 1.e)
                    if_("a".e eq 10.e) {
                        break_()
                    }
                    if_("a".e eq 8.e) {
                        continue_()
                    }
                    "a".e plusAssign 1.e
                }.while_("a".e lt 20.e)
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        do_ {
                            e("a.0".e plusAssign 1.e)
                            if_("a.0".e eq 5.e) {
                                breakLoop(0)
                            }
                            if_("a.0".e eq 3.e) {
                                continue_(0)
                            }
                        }.while_("a.0".e lt 10.e, loopId = 0)
                        do_ {
                            e("a.0".e plusAssign 1.e)
                            if_("a.0".e eq 10.e) {
                                breakLoop(1)
                            }
                            if_("a.0".e eq 8.e) {
                                continue_(1)
                            }
                        }.while_("a.0".e lt 20.e, loopId = 1)
                        return_("a.0".e)
                    }
                },
                variableCount = 1,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue for nested do whiles`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                do_ {
                    e("a".e plusAssign 1.e)
                    do_ {
                        e("a".e plusAssign 2.e)
                        if_("a".e gt 10.e) {
                            break_()
                        }
                    }.while_(1.e)
                    if_("a".e gt 20.e) {
                        break_()
                    }
                }.while_(1.e)
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        do_ {
                            e("a.0".e plusAssign 1.e)
                            do_ {
                                e("a.0".e plusAssign 2.e)
                                if_("a.0".e gt 10.e) {
                                    breakLoop(1)
                                }
                            }.while_(1.e, loopId = 1)
                            if_("a.0".e gt 20.e) {
                                breakLoop(0)
                            }
                        }.while_(1.e, loopId = 0)
                        return_("a.0".e)
                    }
                },
                variableCount = 1,
            ).right(),
            actual = actual,
        )
    }
}
