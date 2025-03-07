package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.c
import koticc.ast.e
import koticc.ast.int
import koticc.ast.program
import koticc.token.Location
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtSwitchTest {
    @Test
    fun `should resolve switch and case ids`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                switch(1.e) {
                    case(1.e) {
                        return_(1.e)
                    }
                    case(2.e) {
                        assign("a".e, 2.e)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        switch(1.e.int(), switchId = 0, hasDefault = true, caseExpressions = mapOf(1.c to 0, 2.c to 1)) {
                            case(1.e.int(), caseId = 0, switchId = 0) {
                                return_(1.e.int())
                            }
                            case(2.e.int(), caseId = 1, switchId = 0) {
                                assign("a.0".e.int(), 2.e.int(), type = Type.Int)
                            }
                            return_("a.0".e.int())
                            default(switchId = 0) {
                                return_(3.e.int())
                            }
                        }
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
    fun `switch without any cases`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        switch(1.e.int(), switchId = 0, caseExpressions = emptyMap(), hasDefault = false) {
                            return_(1.e.int())
                        }
                        return_(0.e.int())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for case outside of switch`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        switch(1.e.int(), switchId = 0, caseExpressions = mapOf(1.c to 0, 2.c to 1), hasDefault = false) {
                            case(1.e.int(), caseId = 0, switchId = 0) {
                                return_(1.e.int())
                            }
                            while_(1.e.int(), loopId = 0) {
                                breakLoop(0)
                                switch(2.e.int(), switchId = 1, caseExpressions = mapOf(3.c to 0, 4.c to 1), hasDefault = true) {
                                    case(3.e.int(), caseId = 0, switchId = 1) {
                                        return_(1.e.int())
                                    }
                                    case(4.e.int(), caseId = 1, switchId = 1) {
                                        breakSwitch(1)
                                    }
                                    default(switchId = 1) {
                                        return_(10.e.int())
                                    }
                                }
                            }
                            case(2.e.int(), caseId = 1, switchId = 0) {
                                breakSwitch(0)
                            }
                        }
                        do_ {
                            breakLoop(1)
                        }.while_(1.e.int(), loopId = 1)
                        return_(0.e.int())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error for duplicate default`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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

    @Test
    fun `should convert case expressions to switch expression type`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                switch("a".e) {
                    case(1L.e) {
                        return_(1.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        switch("a.0".e.int(), switchId = 0, caseExpressions = mapOf(1.c to 0), hasDefault = false) {
                            case(1.e.int(), caseId = 0, switchId = 0) {
                                return_(1.e.int())
                            }
                        }
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
    fun `should return error if case expression is a duplicate after conversion`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                switch("a".e) {
                    // this is the same as -1 after conversion to int
                    case(9223372036854775807L.e) {
                        return_(1.e)
                    }
                    case((-1).e) {
                        return_(2.e)
                    }
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("duplicate case expression: -1", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if switch expression is a double`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                switch(1.0.e) {
                    return_(1.e)
                }
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError("switch expression must have an integer type, got double", DUMMY_LOCATION).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if case expression is a double`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                switch(1.e) {
                    case(1.0.e) {
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
}
