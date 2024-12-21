package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.plus
import koticc.ast.program
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticAnalysisKtStaticVariables {
    @Test
    fun `should pass static specifier`() {
        val program = program {
            function("foo") {
                int("i", storageClass = AST.StorageClass.Static) assign 1.e
                plusAssign("i", 1.e)
                return_("i".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo") {
                        int("i.0", storageClass = AST.StorageClass.Static) assign 1.e
                        plusAssign("i.0", 1.e)
                        return_("i.0".e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameterCount = 0).toSymbol(),
                    "i.0" to Type.Integer.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(1),
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
                value = program,
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "x" to Type.Integer.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(5),
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
            function("read_x") {
                return_("x".e)
            }
            function("update_x", "new_val") {
                // this refers to x already in scope due to extern
                int("x", storageClass = AST.StorageClass.Extern)
                assign("x", "new_val".e)
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
                    function("read_x") {
                        return_("x".e)
                    }
                    function("update_x", "new_val.0") {
                        // this refers to x already in scope due to extern
                        int("x", storageClass = AST.StorageClass.Extern)
                        assign("x", "new_val.0".e)
                    }
                    // x is already in scope, so this refers to that x
                    int("x", storageClass = AST.StorageClass.Extern)
                    int("x", storageClass = AST.StorageClass.Static) assign 5.e
                    // tentative definition can follow a definition with an initializer
                    int("x", storageClass = AST.StorageClass.Static)
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "x" to Type.Integer.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(5),
                            global = false,
                        ),
                    ),
                    "read_x" to Type.Function(parameterCount = 0).toSymbol(),
                    "update_x" to Type.Function(parameterCount = 1).toSymbol(),
                    "new_val.0" to Type.Integer.toSymbol(),
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
            function("foo") {
                int("i", storageClass = AST.StorageClass.Static)
                return_("i".e)
            }
        }

        val actual = semanticAnalysis(program)

        assertEquals(
            expected = ValidASTProgram(
                value = program {
                    function("foo") {
                        int("i.0", storageClass = AST.StorageClass.Static)
                        return_("i.0".e)
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameterCount = 0).toSymbol(),
                    "i.0" to Type.Integer.toSymbol(
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(0),
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
            function("foo") {
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
}
