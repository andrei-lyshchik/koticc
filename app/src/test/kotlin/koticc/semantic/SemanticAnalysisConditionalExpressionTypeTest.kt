package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.cond
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisConditionalExpressionTypeTest {
    @Test
    fun `should resolve type for conditional expression`() {
        val program = program {
            function("main", type = Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                long("b") assign 2L.e
                long("c") assign cond(
                    condition = 1.e,
                    thenExpression = "a".e,
                    elseExpression = "b".e,
                )
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", type = Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign 1.e.int()
                        long("b.1") assign 2L.e.long()
                        long("c.2") assign cond(
                            condition = 1.e.int(),
                            thenExpression = cast(Type.Long, "a.0".e.int()).long(),
                            elseExpression = "b.1".e.long(),
                        ).long()
                    }
                },
                renamedVariableCount = 3,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "b.1" to Type.Long.toSymbol(),
                    "c.2" to Type.Long.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }
}
