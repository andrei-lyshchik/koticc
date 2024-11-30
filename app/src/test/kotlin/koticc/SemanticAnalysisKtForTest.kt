package koticc

import arrow.core.right
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtForTest {
    @Test
    fun `should assign labels to for`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    e("a".e plusAssign "i".e)
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        for_(
                            initDecl("i.1", 0.e),
                            "i.1".e lt 10.e,
                            "i.1".e plusAssign 1.e,
                            loopId = 0,
                        ) {
                            e("a.0".e plusAssign "i.1".e)
                        }
                        return_("a.0".e)
                    }
                },
                variableCount = 2,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels for break continue inside for`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    e("a".e plusAssign "i".e)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                }
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    e("a".e plusAssign "i".e)
                    if_("a".e eq 20.e) {
                        break_()
                    }
                    if_("a".e eq 10.e) {
                        continue_()
                    }
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        for_(
                            initDecl("i.1", 0.e),
                            "i.1".e lt 10.e,
                            "i.1".e plusAssign 1.e,
                            loopId = 0,
                        ) {
                            e("a.0".e plusAssign "i.1".e)
                            if_("a.0".e eq 5.e) {
                                breakLoop(0)
                            }
                            if_("a.0".e eq 3.e) {
                                continue_(0)
                            }
                        }
                        for_(
                            initDecl("i.2", 0.e),
                            "i.2".e lt 10.e,
                            "i.2".e plusAssign 1.e,
                            loopId = 1,
                        ) {
                            e("a.0".e plusAssign "i.2".e)
                            if_("a.0".e eq 20.e) {
                                breakLoop(1)
                            }
                            if_("a.0".e eq 10.e) {
                                continue_(1)
                            }
                        }
                        return_("a.0".e)
                    }
                },
                variableCount = 3,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign loop labels for nested fors`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    e("a".e plusAssign "i".e)
                    for_(
                        initDecl("j", 0.e),
                        "j".e lt 10.e,
                        "j".e plusAssign 1.e,
                    ) {
                        e("a".e plusAssign "j".e)
                        if_("a".e eq 5.e) {
                            break_()
                        }
                        if_("a".e eq 3.e) {
                            continue_()
                        }
                    }
                    if_("a".e eq 20.e) {
                        break_()
                    }
                    if_("a".e eq 10.e) {
                        continue_()
                    }
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        for_(
                            initDecl("i.1", 0.e),
                            "i.1".e lt 10.e,
                            "i.1".e plusAssign 1.e,
                            loopId = 0,
                        ) {
                            e("a.0".e plusAssign "i.1".e)
                            for_(
                                initDecl("j.2", 0.e),
                                "j.2".e lt 10.e,
                                "j.2".e plusAssign 1.e,
                                loopId = 1,
                            ) {
                                e("a.0".e plusAssign "j.2".e)
                                if_("a.0".e eq 5.e) {
                                    breakLoop(1)
                                }
                                if_("a.0".e eq 3.e) {
                                    continue_(1)
                                }
                            }
                            if_("a.0".e eq 20.e) {
                                breakLoop(0)
                            }
                            if_("a.0".e eq 10.e) {
                                continue_(0)
                            }
                        }
                        return_("a.0".e)
                    }
                },
                variableCount = 3,
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should create nested scope in for header`() {
        val input = program {
            func("main") {
                int("a") assign 1.e
                int("i") assign 2.e
                for_(
                    // this is a different scope, so the variable i is different from the one outside
                    initDecl("i", 3.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    // this is again a nested scope,
                    // so the variable i is different from the one outside and the one in the header
                    int("i") assign 4.e
                    e("a".e plusAssign "i".e)
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    func("main") {
                        int("a.0") assign 1.e
                        int("i.1") assign 2.e
                        for_(
                            initDecl("i.2", 3.e),
                            "i.2".e lt 10.e,
                            "i.2".e plusAssign 1.e,
                            loopId = 0,
                        ) {
                            int("i.3") assign 4.e
                            e("a.0".e plusAssign "i.3".e)
                        }
                        return_("a.0".e)
                    }
                },
                variableCount = 4,
            ).right(),
            actual = actual,
        )
    }
}
