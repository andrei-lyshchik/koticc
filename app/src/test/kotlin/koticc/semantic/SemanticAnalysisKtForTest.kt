package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.initDecl
import koticc.ast.int
import koticc.ast.lt
import koticc.ast.plusAssign
import koticc.ast.program
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.EnumSource
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtForTest {
    @Test
    fun `should assign labels to for`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    plusAssign("a".e, "i".e, rightSideType = Type.Int)
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
                        for_(
                            initDecl("i.1", 0.e.int()),
                            ("i.1".e.int() lt 10.e.int()).int(),
                            ("i.1".e.int() plusAssign 1.e.int()).int(),
                            loopId = 0,
                        ) {
                            plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                        }
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "i.1" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign labels for break continue inside for`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    plusAssign("a".e, "i".e, rightSideType = Type.Int)
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
                    plusAssign("a".e, "i".e, rightSideType = Type.Int)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        for_(
                            initDecl("i.1", 0.e.int()),
                            ("i.1".e.int() lt 10.e.int()).int(),
                            ("i.1".e.int() plusAssign 1.e.int()).int(),
                            loopId = 0,
                        ) {
                            plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                            if_(("a.0".e.int() eq 5.e.int()).int()) {
                                breakLoop(0)
                            }
                            if_(("a.0".e.int() eq 3.e.int()).int()) {
                                continue_(0)
                            }
                        }
                        for_(
                            initDecl("i.2", 0.e.int()),
                            ("i.2".e.int() lt 10.e.int()).int(),
                            ("i.2".e.int() plusAssign 1.e.int()).int(),
                            loopId = 1,
                        ) {
                            plusAssign("a.0".e.int(), "i.2".e.int(), resultType = Type.Int, Type.Int)
                            if_(("a.0".e.int() eq 20.e.int()).int()) {
                                breakLoop(1)
                            }
                            if_(("a.0".e.int() eq 10.e.int()).int()) {
                                continue_(1)
                            }
                        }
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 3,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "i.1" to Type.Int.toSymbol(),
                    "i.2" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should assign loop labels for nested fors`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                for_(
                    initDecl("i", 0.e),
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    plusAssign("a".e, "i".e, rightSideType = Type.Int)
                    for_(
                        initDecl("j", 0.e),
                        "j".e lt 10.e,
                        "j".e plusAssign 1.e,
                    ) {
                        plusAssign("a".e, "j".e, rightSideType = Type.Int)
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
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        for_(
                            initDecl("i.1", 0.e.int()),
                            ("i.1".e.int() lt 10.e.int()).int(),
                            ("i.1".e.int() plusAssign 1.e.int()).int(),
                            loopId = 0,
                        ) {
                            plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                            for_(
                                initDecl("j.2", 0.e.int()),
                                ("j.2".e.int() lt 10.e.int()).int(),
                                ("j.2".e.int() plusAssign 1.e.int()).int(),
                                loopId = 1,
                            ) {
                                plusAssign("a.0".e.int(), "j.2".e.int(), resultType = Type.Int, Type.Int)
                                if_(("a.0".e.int() eq 5.e.int()).int()) {
                                    breakLoop(1)
                                }
                                if_(("a.0".e.int() eq 3.e.int()).int()) {
                                    continue_(1)
                                }
                            }
                            if_(("a.0".e.int() eq 20.e.int()).int()) {
                                breakLoop(0)
                            }
                            if_(("a.0".e.int() eq 10.e.int()).int()) {
                                continue_(0)
                            }
                        }
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 3,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "i.1" to Type.Int.toSymbol(),
                    "j.2" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should create nested scope in for header`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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
                    plusAssign("a".e, "i".e, rightSideType = Type.Int)
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
                        int("i.1") assign 2.e.int()
                        for_(
                            initDecl("i.2", 3.e.int()),
                            ("i.2".e.int() lt 10.e.int()).int(),
                            ("i.2".e.int() plusAssign 1.e.int()).int(),
                            loopId = 0,
                        ) {
                            int("i.3") assign 4.e.int()
                            plusAssign("a.0".e.int(), "i.3".e.int(), resultType = Type.Int, Type.Int)
                        }
                        return_("a.0".e.int())
                    }
                },
                renamedVariableCount = 4,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "i.1" to Type.Int.toSymbol(),
                    "i.2" to Type.Int.toSymbol(),
                    "i.3" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @ParameterizedTest
    @EnumSource(AST.StorageClass::class)
    fun `can't use static or extern in for initializer`(storageClass: AST.StorageClass) {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                for_(
                    initDecl("i", 3.e).let {
                        it.copy(
                            declaration = it.declaration.copy(storageClass = storageClass),
                        )
                    },
                    "i".e lt 10.e,
                    "i".e plusAssign 1.e,
                ) {
                    return_("i".e)
                }
            }
        }

        val actual = semanticAnalysis(input)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "can't use storage class specifier in for loop initializer",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }
}
