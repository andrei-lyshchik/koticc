package koticc.tacky

import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.program
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
}
