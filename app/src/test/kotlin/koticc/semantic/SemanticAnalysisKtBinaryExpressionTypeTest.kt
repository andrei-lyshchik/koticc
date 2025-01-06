package koticc.semantic

import arrow.core.right
import koticc.ast.Type
import koticc.ast.and
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.int
import koticc.ast.long
import koticc.ast.or
import koticc.ast.plus
import koticc.ast.program
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtBinaryExpressionTypeTest {
    @Test
    fun `should resolve simple binary expression type, single type case`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e + 2L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign (1L.e.long() + 2L.e.long()).long()
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
    fun `should resolve simple binary expression type, with cast needed for left`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1.e + 2L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign (cast(Type.Long, 1.e.int()).long() + 2L.e.long()).long()
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
    fun `should resolve simple binary expression type, with cast needed for right`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1L.e + 2.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("a.0") assign (1L.e.long() + cast(Type.Long, 2.e.int()).long()).long()
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
    fun `should resolve and expression type`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1L.e and 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign (1L.e.long() and 2.e.int()).int()
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

    @Test
    fun `should resolve or expression type`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1L.e or 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign (1L.e.long() or 2.e.int()).int()
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

    @Test
    fun `should resolve simple binary expression type with boolean result, cast needed`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1.e eq 2L.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign (cast(Type.Long, 1.e.int()).long() eq 2L.e.long()).int()
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
