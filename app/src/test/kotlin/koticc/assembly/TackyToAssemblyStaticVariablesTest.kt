package koticc.assembly

import koticc.semantic.InitialValue
import koticc.semantic.Type
import koticc.semantic.VariableAttributes
import koticc.semantic.toIdentifier
import koticc.tacky.plus
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tackyProgramToAssemblyString
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TackyToAssemblyStaticVariablesTest {
    @Test
    fun `should generate assembly for static variables`() {
        val tacky = tackyProgram {
            staticVariable("a", global = true, initialValue = 0)
            staticVariable("b", global = false, initialValue = 1)
        }

        val assembly = tackyProgramToAssemblyString(tacky)

        assertEquals(
            expected = """
                .globl _a
                .bss
                .balign 4
            _a:
                .zero 4
            
                .data
                .balign 4
            _b:
                .long 1
            """.trimIndent(),
            actual = assembly,
        )
    }

    @Test
    fun `should properly refer to static variables relative to rip`() {
        val tacky = tackyProgram {
            staticVariable("a", global = true, initialValue = 0)
            function("main") {
                assign("a", "a".t + 1.t)
                return_("a".t)
            }
        }

        val assembly = tackyProgramToAssemblyString(
            tacky,
            typedIdentifiers = mapOf(
                "a" to Type.Integer.toIdentifier(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(0), global = true)),
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
            ),
        )

        // since .data/.bss section are memory, we also need to copy the value to a register first in mov instructions, etc
        assertEquals(
            expected = """
                    .globl _a
                    .bss
                    .balign 4
                _a:
                    .zero 4

                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $0, %rsp
                    movl _a(%rip), %r10d
                    movl %r10d, _a(%rip)
                    addl $1, _a(%rip)
                    movl _a(%rip), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = assembly,
        )
    }
}
