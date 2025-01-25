package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
import koticc.ast.plus
import koticc.ast.program
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtStaticVariables {
    @Test
    fun `should pass static specifier`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("i", storageClass = AST.StorageClass.Static) assign 1.e
                plusAssign("i".e, 1.e, rightSideType = Type.Int)
                return_("i".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("i.0", storageClass = AST.StorageClass.Static) assign 1.e.int()
                        plusAssign("i.0".e.int(), 1.e.int(), resultType = Type.Int, Type.Int)
                        return_("i.0".e.int())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "i.0" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(1)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can declare static variables multiple times`() {
        val program = program {
            int("x", storageClass = AST.StorageClass.Static)
            // x is already in scope, so this refers to that x
            int("x", storageClass = AST.StorageClass.Extern)
            int("x", storageClass = AST.StorageClass.Static) assign 5.e
            // tentative definition can follow a definition with an initializer
            int("x", storageClass = AST.StorageClass.Static)
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("x", storageClass = AST.StorageClass.Static)
                    int("x", storageClass = AST.StorageClass.Extern)
                    int("x", storageClass = AST.StorageClass.Static) assign 5.e.int()
                    int("x", storageClass = AST.StorageClass.Static)
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "x" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(5)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can refer to static variables from functions with extern`() {
        val program = program {
            int("x", storageClass = AST.StorageClass.Static)
            function("read_x", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                return_("x".e)
            }
            function("update_x", Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "new_val") {
                // this refers to x already in scope due to extern
                int("x", storageClass = AST.StorageClass.Extern)
                assign("x".e, "new_val".e)
            }
            // x is already in scope, so this refers to that x
            int("x", storageClass = AST.StorageClass.Extern)
            int("x", storageClass = AST.StorageClass.Static) assign 5.e
            // tentative definition can follow a definition with an initializer
            int("x", storageClass = AST.StorageClass.Static)
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("x", storageClass = AST.StorageClass.Static)
                    function("read_x", type = Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        return_("x".e.int())
                    }
                    function("update_x", type = Type.Function(parameters = listOf(Type.Int), returnType = Type.Int), "new_val.0") {
                        // this refers to x already in scope due to extern
                        int("x", storageClass = AST.StorageClass.Extern)
                        assign("x".e.int(), "new_val.0".e.int(), type = Type.Int)
                    }
                    // x is already in scope, so this refers to that x
                    int("x", storageClass = AST.StorageClass.Extern)
                    int("x", storageClass = AST.StorageClass.Static) assign 5.e.int()
                    // tentative definition can follow a definition with an initializer
                    int("x", storageClass = AST.StorageClass.Static)
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "x" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(5)),
                            global = false,
                        ),
                    ),
                    "read_x" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "update_x" to Type.Function(parameters = listOf(Type.Int), returnType = Type.Int).toSymbol(),
                    "new_val.0" to Type.Int.toSymbol(),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can't declare static variable if there's already non-static variable in scope`() {
        val program = program {
            int("x")
            int("x", storageClass = AST.StorageClass.Static)
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "static variable 'x' declaration follows non-static at line 0, column 0",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `if local static variable doesn't have initializer it is initialized to 0`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("i", storageClass = AST.StorageClass.Static)
                return_("i".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        int("i.0", storageClass = AST.StorageClass.Static)
                        return_("i.0".e.int())
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "i.0" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(0)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `can't have non local initializers for local static variables`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("i", storageClass = AST.StorageClass.Static) assign (1.e + 2.e)
                return_("i".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = SemanticAnalysisError(
                message = "non-constant initializer for local static variable 'i'",
                location = DUMMY_LOCATION,
            ).left(),
            actual = actual,
        )
    }

    @Test
    fun `should convert initializers from long to int`() {
        val program = program {
            int("foo", storageClass = AST.StorageClass.Static) assign 2147483648L.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    int("foo", storageClass = AST.StorageClass.Static) assign (-2147483648).e.int()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "foo" to Type.Int.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Int(-2147483648)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should convert initializers from int to long`() {
        val program = program {
            long("foo", storageClass = AST.StorageClass.Static) assign 1.e
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    long("foo", storageClass = AST.StorageClass.Static) assign 1L.e.long()
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "foo" to Type.Long.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Long(1L)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }

    @Test
    fun `should support non-int block static variables`() {
        val program = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("i", storageClass = AST.StorageClass.Static) assign 1.e
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                        long("i.0", storageClass = AST.StorageClass.Static) assign 1L.e.long()
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "i.0" to Type.Long.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(InitialConstantValue.Long(1L)),
                            global = false,
                        ),
                    ),
                ),
            ).right(),
            actual = actual,
        )
    }
}
