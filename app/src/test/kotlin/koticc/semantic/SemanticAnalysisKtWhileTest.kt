package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.gt
import koticc.ast.lt
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtWhileTest {
    @Test
    fun `should assign labels to while`() {
        val input = program {
            function("main") {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a", 1.e)
                }
                while_("a".e lt 40.e) {
                    plusMultiply("a", 2.e)
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main") {
                        int("a.0") assign 1.e
                        while_("a.0".e lt 10.e, loopId = 0) {
                            plusAssign("a.0", 1.e)
                        }
                        while_("a.0".e lt 40.e, loopId = 1) {
                            plusMultiply("a.0", 2.e)
                        }
                        return_("a.0".e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue inside while`() {
        val input = program {
            function("main") {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a", 1.e)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                }
                while_("a".e lt 40.e) {
                    plusAssign("a", 1.e)
                    if_("a".e eq 10.e) {
                        break_()
                    }
                    if_("a".e eq 8.e) {
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
                    function("main") {
                        int("a.0") assign 1.e
                        while_("a.0".e lt 10.e, loopId = 0) {
                            plusAssign("a.0", 1.e)
                            if_("a.0".e eq 5.e) {
                                breakLoop(0)
                            }
                            if_("a.0".e eq 3.e) {
                                continue_(0)
                            }
                        }
                        while_("a.0".e lt 40.e, loopId = 1) {
                            plusAssign("a.0", 1.e)
                            if_("a.0".e eq 10.e) {
                                breakLoop(1)
                            }
                            if_("a.0".e eq 8.e) {
                                continue_(1)
                            }
                        }
                        return_("a.0".e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue for nested whiles`() {
        val input = program {
            function("main") {
                int("a") assign 1.e
                while_(1.e) {
                    plusAssign("a", 1.e)
                    while_(1.e) {
                        plusAssign("a", 2.e)
                        if_("a".e gt 10.e) {
                            break_()
                        }
                    }
                    if_("a".e gt 20.e) {
                        break_()
                    }
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main") {
                        int("a.0") assign 1.e
                        while_(1.e, loopId = 0) {
                            plusAssign("a.0", 1.e)
                            while_(1.e, loopId = 1) {
                                plusAssign("a.0", 2.e)
                                if_("a.0".e gt 10.e) {
                                    breakLoop(1)
                                }
                            }
                            if_("a.0".e gt 20.e) {
                                breakLoop(0)
                            }
                        }
                        return_("a.0".e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
