package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.DUMMY_LOCATION
import koticc.ast.Type
import koticc.ast.e
import koticc.ast.integer
import koticc.ast.program
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

class SemanticAnalysisKtGotoLabelTest {
    @Test
    fun `should handle labeled statement`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                int("a") assign 1.e
                label("label") {
                    assign("a".e, 2.e)
                }
            }
        }

        val expected =
            ValidASTProgram(
                value =
                program {
                    function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                        int("a.0") assign 1.e.integer()
                        label("main.label") {
                            assign("a.0".e.integer(), 2.e.integer(), type = Type.Integer)
                        }
                    }
                },
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                    "a.0" to Type.Integer.toSymbol(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should return error if goto label is not declared`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                goto("label")
            }
        }

        val expected = SemanticAnalysisError("goto to undeclared label 'label'", DUMMY_LOCATION)

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should return error if labels on labeled statements are not unique`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                label("label") { null_() }
                label("label") { null_() }
            }
        }

        val expected = SemanticAnalysisError("label 'label' already declared at line 0, column 0", DUMMY_LOCATION)

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should detect non-unique labels in default`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                label("label") { null_() }
                switch(1.e) {
                    default {
                        label("label") { null_() }
                    }
                }
            }
        }

        val expected = SemanticAnalysisError("label 'label' already declared at line 0, column 0", DUMMY_LOCATION)

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should consider labels inside if else`() {
        val input = program {
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                if_(1.e) {
                    label("label_if") { null_() }
                } else_ {
                    label("label_else") { null_() }
                }
                goto("label_if")
                goto("label_else")
            }
        }

        val expected =
            ValidASTProgram(
                value = program {
                    function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                        if_(1.e.integer()) {
                            label("foo.label_if") { null_() }
                        } else_ {
                            label("foo.label_else") { null_() }
                        }
                        goto("foo.label_if")
                        goto("foo.label_else")
                    }
                },
                renamedVariableCount = 0,
                symbolTable = mapOf(
                    "foo" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should consider non top level gotos`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                if_(1.e) {
                    goto("non_existing_label")
                } else_ {
                    null_()
                }
            }
        }

        val expected = SemanticAnalysisError("goto to undeclared label 'non_existing_label'", DUMMY_LOCATION)

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should find goto and labels in compound statements`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                nested {
                    label("label1") { null_() }
                    goto("label2")
                }
                goto("label1")
                label("label2") { null_() }
            }
        }

        val expected = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    nested {
                        label("main.label1") { null_() }
                        goto("main.label2")
                    }
                    goto("main.label1")
                    label("main.label2") { null_() }
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `labels can be non-unique if used in different functions`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                label("label") { null_() }
            }
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                label("label") { null_() }
            }
        }

        val expected = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    label("main.label") { null_() }
                }
                function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                    label("foo.label") { null_() }
                }
            },
            renamedVariableCount = 0,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
                "foo" to Type.Function(parameters = emptyList(), returnType = Type.Integer).toSymbol(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `can't goto label in different function`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                goto("label")
            }
            function("foo", Type.Function(parameters = emptyList(), returnType = Type.Integer)) {
                label("label") { null_() }
            }
        }

        val expected = SemanticAnalysisError("goto to undeclared label 'label'", DUMMY_LOCATION)

        assertEquals(expected.left(), semanticAnalysis(input))
    }
}
