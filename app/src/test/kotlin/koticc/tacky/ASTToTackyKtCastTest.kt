package koticc.tacky

import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.double
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.program
import koticc.ast.uInt
import koticc.ast.uLong
import koticc.semantic.ValidASTProgram
import koticc.semantic.tempVariablesSymbolTable
import koticc.semantic.toSymbol
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ASTToTackyKtCastTest {
    @Test
    fun `should generate tacky for cast from int to long`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a") assign cast(Type.Long, 1.e.int()).long()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Long.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Long)

                function("foo") {
                    signExtend(1.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from long to int`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign cast(Type.Int, 1.e.long()).int()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Int)

                function("foo") {
                    truncate(1.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned int to int`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign cast(Type.Int, 1u.e.uInt()).int()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Int)

                function("foo") {
                    assign("tmp.1", 1u.t)
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned int to unsigned long`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uLong("a") assign cast(Type.ULong, 1u.e.uInt()).uLong()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.ULong.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.ULong)

                function("foo") {
                    zeroExtend(1u.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned long to unsigned int`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uInt("a") assign cast(Type.UInt, 1.toULong().e.uLong()).uInt()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.UInt.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.UInt)

                function("foo") {
                    truncate(1.toULong().t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned int to long`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a") assign cast(Type.Long, 1u.e.uInt()).long()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Long.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Long)

                function("foo") {
                    zeroExtend(1u.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned long to int`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign cast(Type.Int, 1.toULong().e.uLong()).int()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Int)

                function("foo") {
                    truncate(1.toULong().t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from double to int`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a") assign cast(Type.Int, 1.0.e.double()).int()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Int)

                function("foo") {
                    doubleToInt(1.0.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from double to unsigned long`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    uLong("a") assign cast(Type.ULong, 1.0.e.double()).uLong()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.ULong.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.ULong)

                function("foo") {
                    doubleToUInt(1.0.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from long to double`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    double("a") assign cast(Type.Double, 1L.e.long()).double()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Double.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Double)

                function("foo") {
                    intToDouble(1L.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }

    @Test
    fun `should generate tacky for cast from unsigned int to double`() {
        val program = ValidASTProgram(
            value = program {
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    double("a") assign cast(Type.Double, 1u.e.uInt()).double()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Double.toSymbol(),
            ),
        )

        val actual = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable + tempVariablesSymbolTable(1, 1, type = Type.Double)

                function("foo") {
                    uIntToDouble(1u.t, "tmp.1")
                    assign("a", "tmp.1".t)

                    return_(0.t)
                }
            },
            actual = actual,
        )
    }
}
