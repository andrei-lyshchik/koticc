package koticc.assembly

import koticc.tacky.call
import koticc.tacky.plus
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tackyProgramToAssemblyString
import kotlin.test.Test
import kotlin.test.assertEquals

class TackyToAssemblyKtFunctionsTest {
    @Test
    fun `should support multiple functions`() {
        val program = tackyProgram {
            function("foo") {
                i(1.t + 2.t assignTo "tmp.1".t)
                i("tmp.1".t + 1.t assignTo "tmp.2".t)
                i("tmp.2".t + 1.t assignTo "tmp.3".t)
                i("tmp.3".t + 1.t assignTo "tmp.4".t)
                i("tmp.4".t + 1.t assignTo "tmp.5".t)
                i("tmp.5".t + 1.t assignTo "tmp.6".t)
                return_("tmp.6".t)
            }
            function("bar") {
                i(2.t + 3.t assignTo "tmp.2".t)
                i("tmp.2".t + 1.t assignTo "tmp.3".t)
                return_("tmp.3".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        // despite functions needing fewer stack slots, we always allocate in multiple of 16-bytes, to make stack alignment during function
        // calls easier
        assertEquals(
            expected = """
                    .globl _foo
                _foo:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $32, %rsp
                    movl $1, -4(%rbp)
                    addl $2, -4(%rbp)
                    movl -4(%rbp), %r10d
                    movl %r10d, -8(%rbp)
                    addl $1, -8(%rbp)
                    movl -8(%rbp), %r10d
                    movl %r10d, -12(%rbp)
                    addl $1, -12(%rbp)
                    movl -12(%rbp), %r10d
                    movl %r10d, -16(%rbp)
                    addl $1, -16(%rbp)
                    movl -16(%rbp), %r10d
                    movl %r10d, -20(%rbp)
                    addl $1, -20(%rbp)
                    movl -20(%rbp), %r10d
                    movl %r10d, -24(%rbp)
                    addl $1, -24(%rbp)
                    movl -24(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret

                    .globl _bar
                _bar:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $2, -4(%rbp)
                    addl $3, -4(%rbp)
                    movl -4(%rbp), %r10d
                    movl %r10d, -8(%rbp)
                    addl $1, -8(%rbp)
                    movl -8(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should support function with a parameter`() {
        val program = tackyProgram {
            function("foo", "a") {
                i("a".t + 1.t assignTo "tmp.1".t)
                return_("tmp.1".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _foo
                _foo:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl %edi, -4(%rbp)
                    movl -4(%rbp), %r10d
                    movl %r10d, -8(%rbp)
                    addl $1, -8(%rbp)
                    movl -8(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should support function with many parameters`() {
        val program = tackyProgram {
            function("foo", "a", "b", "c", "d", "e", "f", "g", "h") {
                i("a".t + "h".t assignTo "tmp.1".t)
                return_("tmp.1".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _foo
                _foo:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $48, %rsp
                    movl %edi, -4(%rbp)
                    movl %esi, -8(%rbp)
                    movl %edx, -12(%rbp)
                    movl %ecx, -16(%rbp)
                    movl %r8d, -20(%rbp)
                    movl %r9d, -24(%rbp)
                    movl 16(%rbp), %r10d
                    movl %r10d, -28(%rbp)
                    movl 24(%rbp), %r10d
                    movl %r10d, -32(%rbp)
                    movl -4(%rbp), %r10d
                    movl %r10d, -36(%rbp)
                    movl -32(%rbp), %r10d
                    addl %r10d, -36(%rbp)
                    movl -36(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should support function call with one argument`() {
        val program = tackyProgram {
            function("main") {
                i(call("foo", 1.t) assignTo "tmp.0".t)
                return_("tmp.0".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $1, %edi
                    call _foo
                    movl %eax, -4(%rbp)
                    movl -4(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should support function call with many arguments, even number of stack arguments`() {
        val program = tackyProgram {
            function("main") {
                i(call("foo", 1.t, 2.t, 3.t, 4.t, 5.t, 6.t, 7.t, 8.t) assignTo "tmp.0".t)
                return_("tmp.0".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $1, %edi
                    movl $2, %esi
                    movl $3, %edx
                    movl $4, %ecx
                    movl $5, %r8d
                    movl $6, %r9d
                    pushq $8
                    pushq $7
                    call _foo
                    addq $16, %rsp
                    movl %eax, -4(%rbp)
                    movl -4(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should support function call with many arguments, odd number of stack arguments`() {
        val program = tackyProgram {
            function("main") {
                i(call("foo", 1.t, 2.t, 3.t, 4.t, 5.t, 6.t, 7.t, 8.t, 9.t) assignTo "tmp.0".t)
                return_("tmp.0".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    subq $8, %rsp
                    movl $1, %edi
                    movl $2, %esi
                    movl $3, %edx
                    movl $4, %ecx
                    movl $5, %r8d
                    movl $6, %r9d
                    pushq $9
                    pushq $8
                    pushq $7
                    call _foo
                    addq $32, %rsp
                    movl %eax, -4(%rbp)
                    movl -4(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }

    @Test
    fun `should copy stack arguments first to eax`() {
        val program = tackyProgram {
            function("main") {
                i(1.t + 2.t assignTo "tmp.0".t)
                i(call("foo", 1.t, 2.t, 3.t, 4.t, 5.t, 6.t, "tmp.0".t, 8.t, 9.t) assignTo "tmp.1".t)
                return_("tmp.1".t)
            }
        }

        val actual = tackyProgramToAssemblyString(program)

        assertEquals(
            expected = """
                    .globl _main
                _main:
                    pushq %rbp
                    movq %rsp, %rbp
                    subq $16, %rsp
                    movl $1, -4(%rbp)
                    addl $2, -4(%rbp)
                    subq $8, %rsp
                    movl $1, %edi
                    movl $2, %esi
                    movl $3, %edx
                    movl $4, %ecx
                    movl $5, %r8d
                    movl $6, %r9d
                    pushq $9
                    pushq $8
                    movl -4(%rbp), %eax
                    pushq %rax
                    call _foo
                    addq $32, %rsp
                    movl %eax, -8(%rbp)
                    movl -8(%rbp), %eax
                    movq %rbp, %rsp
                    popq %rbp
                    ret
            """.trimIndent(),
            actual = actual,
        )
    }
}
