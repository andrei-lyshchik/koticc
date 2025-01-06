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

class SemanticAnalysisKtTypecheckTest {
    @Test
    fun `should resolve type for long variable declaration and constant`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign 1L.e.long()
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
    fun `should resolve type for long used variable`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e
                long("b") assign "a".e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign 1L.e.long()
                        long("b.1") assign "a.0".e.long()
                    }
                },
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Long.toSymbol(),
                    "b.1" to Type.Long.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve type for assignment, cast needed`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e
                assign("a".e, 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign 1L.e.long()
                        assign("a.0".e.long(), cast(Type.Long, 2.e.int()).long(), type = Type.Long)
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
    fun `should resolve type for cast`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign cast(Type.Long, 1.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign cast(Type.Long, 1.e.int()).long()
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
