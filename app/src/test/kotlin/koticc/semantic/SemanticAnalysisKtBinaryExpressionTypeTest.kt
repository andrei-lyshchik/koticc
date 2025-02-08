package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.and
import koticc.ast.cast
import koticc.ast.double
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.int
import koticc.ast.long
import koticc.ast.or
import koticc.ast.plus
import koticc.ast.program
import koticc.ast.rem
import koticc.ast.shl
import koticc.ast.uInt
import koticc.ast.uLong
import koticc.ast.xor
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
    fun `should resolve common type for unsigned int and unsigned long to be unsigned long`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                uLong("a") assign 1u.e + 2.toULong().e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        uLong("a.0") assign (cast(Type.ULong, 1u.e.uInt()).uLong() + 2.toULong().e.uLong()).uLong()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.ULong.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for unsigned long and signed int to be unsigned long`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                uLong("a") assign 1.toULong().e + 2.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        uLong("a.0") assign (1.toULong().e.uLong() + cast(Type.ULong, 2.e.int()).uLong()).uLong()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.ULong.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for unsigned int and signed int to be unsigned int`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                uLong("a") assign 1u.e + 2.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        uLong("a.0") assign cast(Type.ULong, (1u.e.uInt() + cast(Type.UInt, 2.e.int()).uInt()).uInt()).uLong()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.ULong.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for unsigned int and signed long to be signed long`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                uLong("a") assign 1u.e + 2L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        uLong("a.0") assign cast(Type.ULong, (cast(Type.Long, 1u.e.uInt()).long() + 2L.e.long()).long()).uLong()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.ULong.toSymbol(),
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
    fun `should resolve common type for double and int to be double`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                double("a") assign 1.0.e + 2.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        double("a.0") assign (1.0.e.double() + cast(Type.Double, 2.e.int()).double()).double()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Double.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for double and long to be double`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                double("a") assign 1.0.e + 2L.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        double("a.0") assign (1.0.e.double() + cast(Type.Double, 2L.e.long()).double()).double()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Double.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for double and unsigned long to be double`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                double("a") assign 1.0.e + 2uL.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        double("a.0") assign (1.0.e.double() + cast(Type.Double, 2uL.e.uLong()).double()).double()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Double.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should resolve common type for double and unsigned int to be double`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                double("a") assign 1.0.e + 2u.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        double("a.0") assign (1.0.e.double() + cast(Type.Double, 2u.e.uInt()).double()).double()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Double.toSymbol(),
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

    @Test
    fun `should resolve shift expression type as left operand type`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1.e shl 2L.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("a.0") assign (1.e.int() shl 2L.e.long()).int()
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
    fun `should not allow double operands for modulo binary operator`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.0.e % 2.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "invalid double operand type for '%'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should not allow double for bitwise xor`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1.0.e xor 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "invalid double operand type for '^'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should not allow double for bitwise shift`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign (1.0.e shl 2.e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "invalid double operand type for '<<'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }
}
