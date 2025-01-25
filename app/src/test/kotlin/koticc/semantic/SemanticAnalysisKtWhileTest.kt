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

class SemanticAnalysisKtWhileTest {
    @Test
    fun `should assign labels to while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a".e, 1.e, rightSideType = Type.Int)
                }
                while_("a".e lt 40.e) {
                    plusMultiply("a".e, 2.e, rightSideType = Type.Int)
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0) {
                            plusAssign("a.0".e.int(), 1.e.int(), resultType = Type.Int, rightSideType = Type.Int)
                        }
                        while_(("a.0".e.int() lt 40.e.int()).int(), loopId = 1) {
                            plusMultiply("a.0".e.int(), 2.e.int(), resultType = Type.Int, rightSideType = Type.Int)
                        }
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
    fun `should assign labels to break continue inside while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a".e, 1.e, rightSideType = Type.Int)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                }
                while_("a".e lt 40.e) {
                    plusAssign("a".e, 1.e, rightSideType = Type.Int)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        while_(("a.0".e.int() lt 10.e.int()).int(), loopId = 0) {
                            plusAssign("a.0".e.int(), 1.e.int(), resultType = Type.Int, Type.Int)
                            if_(("a.0".e.int() eq 5.e.int()).int()) {
                                breakLoop(0)
                            }
                            if_(("a.0".e.int() eq 3.e.int()).int()) {
                                continue_(0)
                            }
                        }
                        while_(("a.0".e.int() lt 40.e.int()).int(), loopId = 1) {
                            plusAssign("a.0".e.int(), 1.e.int(), resultType = Type.Int, Type.Int)
                            if_(("a.0".e.int() eq 10.e.int()).int()) {
                                breakLoop(1)
                            }
                            if_(("a.0".e.int() eq 8.e.int()).int()) {
                                continue_(1)
                            }
                        }
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
    fun `should assign labels to break continue for nested whiles`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                while_(1.e) {
                    plusAssign("a".e, 1.e, rightSideType = Type.Int)
                    while_(1.e) {
                        plusAssign("a".e, 2.e, rightSideType = Type.Int)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        while_(1.e.int(), loopId = 0) {
                            plusAssign("a.0".e.int(), 1.e.int(), resultType = Type.Int, Type.Int)
                            while_(1.e.int(), loopId = 1) {
                                plusAssign("a.0".e.int(), 2.e.int(), resultType = Type.Int, Type.Int)
                                if_(("a.0".e.int() gt 10.e.int()).int()) {
                                    breakLoop(1)
                                }
                            }
                            if_(("a.0".e.int() gt 20.e.int()).int()) {
                                breakLoop(0)
                            }
                        }
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
