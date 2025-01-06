package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.program
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisCompoundAssignmentTypeTest {
    @Test
    fun `should typecheck compound assignment, with cast needed`() {
        val program = program {
            function("foo", type = Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e
                plusAssign("a".e, 2.e, type = Type.Long)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", type = Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign 1L.e.long()
                        plusAssign("a.0".e.long(), cast(Type.Long, 2.e.int()).long(), type = Type.Long)
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
}
