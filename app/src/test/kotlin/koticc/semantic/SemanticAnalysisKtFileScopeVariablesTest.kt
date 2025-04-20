package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.double
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.plus
import koticc.ast.program
import koticc.ast.uInt
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtFileScopeVariablesTest {
    @Test
    fun `should not rename file scope variables`() {
        val program = program {
            int("a")
            int("b") assign 1.e
        }

        val result = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("a")
                    int("b") assign 1.e.int()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Tentative,
                            global = true,
                        ),
                    ),
                    "b" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(1)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = result,
        )
    }

    @Test
    fun `should be possible to refer to variable from outer scope with extern and do it multiple times`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("foo") assign 1.e
                if_("foo".e) {
                    int("foo", storageClass = AST.StorageClass.Extern)
                    int("foo", storageClass = AST.StorageClass.Extern)
                    return_("foo".e)
                }
            }
            int("foo", storageClass = AST.StorageClass.Extern) assign 2.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("foo.0") assign 1.e.int()
                        if_("foo.0".e.int()) {
                            int("foo", storageClass = AST.StorageClass.Extern)
                            int("foo", storageClass = AST.StorageClass.Extern)
                            return_("foo".e.int())
                        }
                    }
                    int("foo", storageClass = AST.StorageClass.Extern) assign 2.e.int()
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "foo.0" to Type.Int.toSymbol(attributes = VariableAttributes.Local),
                    "foo" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(2)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `file-scope variables can't have non constant initializers`() {
        val program = program {
            int("a") assign (1.e + 2.e)
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "non-constant initializer for variable 'a'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare a file-scope variable if function with the same name is already declared`() {
        val program = program {
            function("a", Type.Function(parameters = emptyList(), returnType = Type.Int))
            int("a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "function 'a' redeclared as a variable",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `can't define file scope variables multiple times`() {
        val program = program {
            int("a") assign 1.e
            int("a") assign 2.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "redeclaration of 'a' with a different initializer",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return errors if file scope variable declarations have conflicting types`() {
        val program = program {
            int("a")
            long("a")
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "conflicting types for 'a' at line 0, column 0",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should properly track types of non int extern local variables`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                long("foo", storageClass = AST.StorageClass.Extern)
                return_("foo".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Long)) {
                        long("foo", storageClass = AST.StorageClass.Extern)
                        return_("foo".e.long())
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Long).toSymbol(),
                    "foo" to Type.Long.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.NoInitializer,
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if file scope variable has conflicting type`() {
        val program = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("foo", storageClass = AST.StorageClass.Extern)
                return_("foo".e)
            }
            int("foo", storageClass = AST.StorageClass.Extern) assign 2.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "conflicting types for 'foo' at line 0, column 0",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should return error if local extern variable has conflicting type`() {
        val program = program {
            int("foo", storageClass = AST.StorageClass.Extern) assign 2.e
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("foo", storageClass = AST.StorageClass.Extern)
                return_("foo".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "conflicting types for 'foo' at line 0, column 0",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should support unsigned int constants`() {
        val program = program {
            uInt("a") assign 1u.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    uInt("a") assign 1.toUInt().e.uInt()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.UInt.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.UInt(1u)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should support double constants`() {
        val program = program {
            double("a") assign 3.14.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    double("a") assign 3.14.e.double()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.Double.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Double(3.14)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should convert double constant to int`() {
        val program = program {
            int("a") assign 3.8.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("a") assign 3.e.int()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(3)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should convert long constant to double`() {
        val program = program {
            double("a") assign 3L.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    double("a") assign 3.0.e.double()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "a" to Type.Double.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Double(3.0)),
                            global = true,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }
}
