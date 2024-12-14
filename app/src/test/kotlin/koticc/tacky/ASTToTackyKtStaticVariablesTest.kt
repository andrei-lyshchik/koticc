package koticc.tacky

import koticc.ast.e
import koticc.ast.program
import koticc.semantic.InitialValue
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import koticc.semantic.VariableAttributes
import koticc.semantic.toIdentifier
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ASTToTackyKtStaticVariablesTest {
    @Test
    fun `should generate tacky for global file-level variables`() {
        val program = ValidASTProgram(
            value = program {
                int("a")
                int("b") assign 2.e
                int("c") assign 3.e
            },
            renamedVariableCount = 3,
            typedIdentifiers = mapOf(
                // No initializer => defined somewhere else, so we should skip it
                "a" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.NoInitializer,
                        global = true,
                    ),
                ),
                "b" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Tentative,
                        global = true,
                    ),
                ),
                "c" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(2),
                        global = true,
                    ),
                ),
                "d" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(3),
                        global = false,
                    ),
                ),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                staticVariable("b", global = true, initialValue = 0)
                staticVariable("c", global = true, initialValue = 2)
                staticVariable("d", global = false, initialValue = 3)
            },
            actual = tacky,
        )
    }

    @Test
    fun `should generate tacky for local static variables`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    int("a")
                    int("b") assign 2.e
                    return_("a".e)
                }
            },
            renamedVariableCount = 3,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "a" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Tentative,
                        global = false,
                    ),
                ),
                "b" to Type.Integer.toIdentifier(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(2),
                        global = false,
                    ),
                ),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                function("main") {
                    return_("a".t)
                    return_(0.t)
                }
                staticVariable("a", global = false, initialValue = 0)
                staticVariable("b", global = false, initialValue = 2)
            },
            actual = tacky,
        )
    }
}
