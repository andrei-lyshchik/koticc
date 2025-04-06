package koticc.tacky

import koticc.ast.Type
import koticc.ast.assign
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.initDecl
import koticc.ast.initExpr
import koticc.ast.int
import koticc.ast.lt
import koticc.ast.plusAssign
import koticc.ast.program
import koticc.semantic.ValidASTProgram
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import kotlin.test.Test
import kotlin.test.assertEquals

class ASTToTackyKtForTest {
    @Test
    fun `should produce tacky for for`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    for_(initDecl("i.1", initializer = 0.e.int()), ("i.1".e.int() lt 10.e.int()).int(), ("i.1".e.int() plusAssign 1.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                    }
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
                "i.1" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(2, 3)

                function("main") {
                    assign("a.0", 1.t)

                    assign("i.1", 0.t)
                    label("loop_start.0")

                    assign("tmp.2", "i.1".t lt 10.t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    assign("tmp.3", "a.0".t + "i.1".t)
                    assign("a.0", "tmp.3".t)

                    label("loop_continue.0")
                    assign("tmp.4", "i.1".t + 1.t)
                    assign("i.1", "tmp.4".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without initializer`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e
                    for_(null, ("a.0".e.int() lt 10.e.int()).int(), ("a.0".e.int() plusAssign 1.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), 1.e.int(), resultType = Type.Int, Type.Int)
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(1, 3)

                function("main") {
                    assign("a.0", 1.t)

                    label("loop_start.0")

                    assign("tmp.1", "a.0".t lt 10.t)
                    jumpIfZero("tmp.1".t, "loop_end.0")

                    assign("tmp.2", "a.0".t + 1.t)
                    assign("a.0", "tmp.2".t)

                    label("loop_continue.0")
                    assign("tmp.3", "a.0".t + 1.t)
                    assign("a.0", "tmp.3".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without condition`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    int("i.1")
                    for_(initExpr(("i.1".e assign 0.e.int()).int()), null, ("i.1".e.int() plusAssign 1.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                    }
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
                "i.1" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(2, 2)

                function("main") {
                    assign("a.0", 1.t)

                    assign("i.1", 0.t)
                    label("loop_start.0")

                    assign("tmp.2", "a.0".t + "i.1".t)
                    assign("a.0", "tmp.2".t)

                    label("loop_continue.0")
                    assign("tmp.3", "i.1".t + 1.t)
                    assign("i.1", "tmp.3".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for without post`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e
                    int("i.1")
                    for_(initExpr(("i.1".e.int() assign 0.e.int()).int()), ("i.1".e.int() lt 10.e.int()).int(), null, loopId = 0) {
                        plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                    }
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
                "i.1" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(2, 2)

                function("main") {
                    assign("a.0", 1.t)

                    assign("i.1", 0.t)
                    label("loop_start.0")

                    assign("tmp.2", "i.1".t lt 10.t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    assign("tmp.3", "a.0".t + "i.1".t)
                    assign("a.0", "tmp.3".t)

                    label("loop_continue.0")

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should produce tacky for for with break or continue`() {
        val input = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    for_(initDecl("i.1", initializer = 0.e.int()), ("i.1".e.int() lt 10.e.int()).int(), ("i.1".e.int() plusAssign 1.e.int()).int(), loopId = 0) {
                        plusAssign("a.0".e.int(), "i.1".e.int(), resultType = Type.Int, Type.Int)
                        if_(("i.1".e.int() eq 5.e.int()).int()) {
                            continue_(0)
                        } else_ {
                            breakLoop(0)
                        }
                    }
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
                "i.1" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(input)

        assertEquals(
            expected = tackyProgram {
                symbolTable = input.symbolTable + tempVariablesSymbolTable(2, 4)

                function("main") {
                    assign("a.0", 1.t)

                    assign("i.1", 0.t)
                    label("loop_start.0")

                    assign("tmp.2", "i.1".t lt 10.t)
                    jumpIfZero("tmp.2".t, "loop_end.0")

                    assign("tmp.3", "a.0".t + "i.1".t)
                    assign("a.0", "tmp.3".t)

                    assign("tmp.4", "i.1".t eq 5.t)
                    jumpIfZero("tmp.4".t, "if_else.0")
                    jump("loop_continue.0")
                    jump("if_end.1")
                    label("if_else.0")
                    jump("loop_end.0")
                    label("if_end.1")

                    label("loop_continue.0")
                    assign("tmp.5", "i.1".t + 1.t)
                    assign("i.1", "tmp.5".t)

                    jump("loop_start.0")
                    label("loop_end.0")

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
