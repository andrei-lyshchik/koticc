package koticc.assembly

import koticc.ast.Type
import koticc.semantic.InitialConstantValue
import koticc.semantic.InitialValue
import koticc.semantic.VariableAttributes
import koticc.semantic.toSymbol
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
            symbolTable = mapOf(
                "a" to Type.Int.toSymbol(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(listOf(InitialConstantValue.Int(0))), global = true)),
                "b" to Type.Int.toSymbol(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(listOf(InitialConstantValue.Int(1))), global = false)),
                "c" to Type.Long.toSymbol(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(listOf(InitialConstantValue.Long(0))), global = true)),
                "d" to Type.Long.toSymbol(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(listOf(InitialConstantValue.Long(2))), global = true)),
            )
            staticVariable("a", global = true, initialValue = 0)
            staticVariable("b", global = false, initialValue = 1)
            staticVariable("c", global = true, initialValue = 0L)
            staticVariable("d", global = true, initialValue = 2L)
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
            
                .globl _c
                .bss
                .balign 8
            _c:
                .zero 8
            
                .globl _d
                .data
                .balign 8
            _d:
                .quad 2
            """.trimIndent(),
            actual = assembly,
        )
    }

    @Test
    fun `should properly refer to static variables relative to rip`() {
        val tacky = tackyProgram {
            symbolTable = mapOf(
                "a" to Type.Int.toSymbol(attributes = VariableAttributes.Static(initialValue = InitialValue.Constant(listOf(InitialConstantValue.Int(0))), global = true)),
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
            )

            staticVariable("a", global = true, initialValue = 0)
            function("main") {
                assign("a", "a".t + 1.t)
                return_("a".t)
            }
        }

        val assembly = tackyProgramToAssemblyString(
            tacky,
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
