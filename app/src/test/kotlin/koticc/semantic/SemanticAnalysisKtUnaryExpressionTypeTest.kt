package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.complement
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.not
import koticc.ast.program
import koticc.ast.unaryMinus
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtUnaryExpressionTypeTest {
    @Test
    fun `should resolve type of unary negate expression`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign -(1L.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign (-(1L.e.long())).long()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Long.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve type of unary complement expression`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e.complement()
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign 1L.e.long().complement().long()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Long.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve type of logical negate expression`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign !1L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign (!(1L.e.long())).int()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
