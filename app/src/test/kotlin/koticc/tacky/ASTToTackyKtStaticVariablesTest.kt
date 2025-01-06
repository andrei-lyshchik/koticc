package koticc.tacky

import koticc.ast.Type
import koticc.ast.e
import koticc.ast.program
import koticc.semantic.InitialConstantValue
import koticc.semantic.InitialValue
import koticc.semantic.ValidASTProgram
import koticc.semantic.VariableAttributes
import koticc.semantic.toSymbol
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
            symbolTable = mapOf(
                // No initializer => defined somewhere else, so we should skip it
                "a" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.NoInitializer,
                        global = true,
                    ),
                ),
                "b" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Tentative,
                        global = true,
                    ),
                ),
                "c" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(InitialConstantValue.Int(2)),
                        global = true,
                    ),
                ),
                "d" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(InitialConstantValue.Int(3)),
                        global = false,
                    ),
                ),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable

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
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a")
                    int("b") assign 2.e
                    return_("a".e)
                }
            },
            renamedVariableCount = 3,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Tentative,
                        global = false,
                    ),
                ),
                "b" to Type.Int.toSymbol(
                    attributes = VariableAttributes.Static(
                        initialValue = InitialValue.Constant(InitialConstantValue.Int(2)),
                        global = false,
                    ),
                ),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            expected = tackyProgram {
                symbolTable = program.symbolTable

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
