package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.gt
import koticc.ast.int
import koticc.ast.lt
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtDoWhileTest {
    @Test
    fun `should assign labels to do while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                do_ {
                    plusAssign("a".e, 1.e)
                }.while_("a".e lt 10.e)
                do_ {
                    plusMultiply("a".e, 2.e)
                }.while_("a".e lt 40.e)
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        do_ {
                            plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                        }.while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0)
                        do_ {
                            plusMultiply("a.0".e.int(), 2.e.int(), type = Type.Int)
                        }.while_(("a.0".e.int() lt 40.e.int()).int(), loopId = 1)
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue inside do while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                do_ {
                    plusAssign("a".e, 1.e)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                }.while_("a".e lt 10.e)
                do_ {
                    plusAssign("a".e, 1.e)
                    if_("a".e eq 10.e) {
                        break_()
                    }
                    if_("a".e eq 8.e) {
                        continue_()
                    }
                }.while_("a".e lt 20.e)
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        do_ {
                            plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                            if_(("a.0".e.int() eq 5.e.int()).int()) {
                                breakLoop(0)
                            }
                            if_(("a.0".e.int() eq 3.e.int()).int()) {
                                continue_(0)
                            }
                        }.while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0)
                        do_ {
                            plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                            if_(("a.0".e.int() eq 10.e.int()).int()) {
                                breakLoop(1)
                            }
                            if_(("a.0".e.int() eq 8.e.int()).int()) {
                                continue_(1)
                            }
                        }.while_(("a.0".e.int() lt 20.e.int()).int(), loopId = 1)
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue for nested do whiles`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                do_ {
                    plusAssign("a".e, 1.e)
                    do_ {
                        plusAssign("a".e, 2.e)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        do_ {
                            plusAssign("a.0".e.int(), 1.e.int(), type = Type.Int)
                            do_ {
                                plusAssign("a.0".e.int(), 2.e.int(), type = Type.Int)
                                if_(("a.0".e.int() gt 10.e.int()).int()) {
                                    breakLoop(1)
                                }
                            }.while_(1.e.int(), loopId = 1)
                            if_(("a.0".e.int() gt 20.e.int()).int()) {
                                breakLoop(0)
                            }
                        }.while_(1.e.int(), loopId = 0)
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
