package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.gt
import koticc.ast.integer
import koticc.ast.lt
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtWhileTest {
    @Test
    fun `should assign labels to while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a".e, 1.e)
                }
                while_("a".e lt 40.e) {
                    plusMultiply("a".e, 2.e)
                }
                return_("a".e)
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                        int("a.0") assign 1.e.integer()
                        while_(("a.0".e.integer() lt 10.e.integer()).integer(), loopId = 0) {
                            plusAssign("a.0".e.integer(), 1.e.integer(), type = Type.Integer)
                        }
                        while_(("a.0".e.integer() lt 40.e.integer()).integer(), loopId = 1) {
                            plusMultiply("a.0".e.integer(), 2.e.integer(), type = Type.Integer)
                        }
                        return_("a.0".e.integer())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                    "a.0" to Type.Integer.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue inside while`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                int("a") assign 1.e
                while_("a".e lt 10.e) {
                    plusAssign("a".e, 1.e)
                    if_("a".e eq 5.e) {
                        break_()
                    }
                    if_("a".e eq 3.e) {
                        continue_()
                    }
                }
                while_("a".e lt 40.e) {
                    plusAssign("a".e, 1.e)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                        int("a.0") assign 1.e.integer()
                        while_(("a.0".e.integer() lt 10.e.integer()).integer(), loopId = 0) {
                            plusAssign("a.0".e.integer(), 1.e.integer(), type = Type.Integer)
                            if_(("a.0".e.integer() eq 5.e.integer()).integer()) {
                                breakLoop(0)
                            }
                            if_(("a.0".e.integer() eq 3.e.integer()).integer()) {
                                continue_(0)
                            }
                        }
                        while_(("a.0".e.integer() lt 40.e.integer()).integer(), loopId = 1) {
                            plusAssign("a.0".e.integer(), 1.e.integer(), type = Type.Integer)
                            if_(("a.0".e.integer() eq 10.e.integer()).integer()) {
                                breakLoop(1)
                            }
                            if_(("a.0".e.integer() eq 8.e.integer()).integer()) {
                                continue_(1)
                            }
                        }
                        return_("a.0".e.integer())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                    "a.0" to Type.Integer.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels to break continue for nested whiles`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                int("a") assign 1.e
                while_(1.e) {
                    plusAssign("a".e, 1.e)
                    while_(1.e) {
                        plusAssign("a".e, 2.e)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                        int("a.0") assign 1.e.integer()
                        while_(1.e.integer(), loopId = 0) {
                            plusAssign("a.0".e.integer(), 1.e.integer(), type = Type.Integer)
                            while_(1.e.integer(), loopId = 1) {
                                plusAssign("a.0".e.integer(), 2.e.integer(), type = Type.Integer)
                                if_(("a.0".e.integer() gt 10.e.integer()).integer()) {
                                    breakLoop(1)
                                }
                            }
                            if_(("a.0".e.integer() gt 20.e.integer()).integer()) {
                                breakLoop(0)
                            }
                        }
                        return_("a.0".e.integer())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                    "a.0" to Type.Integer.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
