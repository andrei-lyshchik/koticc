package koticc.assembly

import koticc.VarargArgumentsProvider
import koticc.ast.LabelName
import koticc.semantic.empty
import koticc.tacky.Tacky
import koticc.tacky.and
import koticc.tacky.complement
import koticc.tacky.div
import koticc.tacky.eq
import koticc.tacky.gt
import koticc.tacky.gte
import koticc.tacky.lt
import koticc.tacky.lte
import koticc.tacky.minus
import koticc.tacky.neq
import koticc.tacky.not
import koticc.tacky.or
import koticc.tacky.plus
import koticc.tacky.rem
import koticc.tacky.shl
import koticc.tacky.shr
import koticc.tacky.t
import koticc.tacky.tackyProgram
import koticc.tacky.times
import koticc.tacky.unaryMinus
import koticc.tacky.xor
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class TackyToAssemblyKtTest {
    @Test
    fun `should generate assembly for single return`() {
        val tacky = tackyProgram {
            function("main") {
                return_(42.t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Ret,
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should allocate stack for variables`() {
        val tacky = tackyProgram {
            function("main") {
                assign("x", 42.t)
                return_("x".t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-4),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Ret,
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    class SimpleUnaryTestCases : VarargArgumentsProvider(
        Tacky.UnaryOperator.Negate to Assembly.UnaryOperator.Neg,
        Tacky.UnaryOperator.Complement to Assembly.UnaryOperator.Not,
    )

    @ParameterizedTest
    @ArgumentsSource(SimpleUnaryTestCases::class)
    fun `should generate assembly for simple unary operations`(
        tackyOperator: Tacky.UnaryOperator,
        assemblyOperator: Assembly.UnaryOperator,
    ) {
        val tacky = tackyProgram {
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.UnaryOperator.Negate -> -(42.t)
                        Tacky.UnaryOperator.Complement -> 42.t.complement()
                        Tacky.UnaryOperator.LogicalNegate -> error("Not in this test")
                    },
                )
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Unary(
                                    operator = assemblyOperator,
                                    operand = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate assembly for logical negate`() {
        val tacky = tackyProgram {
            function("main") {
                assign("x", !42.t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(0),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Cmp(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Set(
                                    operator = Assembly.ConditionalOperator.Equal,
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    class SimpleBinaryTestCases : VarargArgumentsProvider(
        Tacky.BinaryOperator.Add to Assembly.BinaryOperator.Add,
        Tacky.BinaryOperator.Subtract to Assembly.BinaryOperator.Sub,
        Tacky.BinaryOperator.BitwiseAnd to Assembly.BinaryOperator.And,
        Tacky.BinaryOperator.BitwiseOr to Assembly.BinaryOperator.Or,
        Tacky.BinaryOperator.BitwiseXor to Assembly.BinaryOperator.Xor,
    )

    @ParameterizedTest
    @ArgumentsSource(SimpleBinaryTestCases::class)
    fun `should generate assembly for simple binary operations`(
        tackyOperator: Tacky.BinaryOperator,
        assemblyOperator: Assembly.BinaryOperator,
    ) {
        val tacky = tackyProgram {
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.Add -> 1.t + 2.t
                        Tacky.BinaryOperator.Subtract -> 1.t - 2.t
                        Tacky.BinaryOperator.BitwiseAnd -> 1.t and 2.t
                        Tacky.BinaryOperator.BitwiseOr -> 1.t or 2.t
                        Tacky.BinaryOperator.BitwiseXor -> 1.t xor 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Binary(
                                    operator = assemblyOperator,
                                    src = Assembly.Operand.Immediate(2),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate assembly for multiplication`() {
        val tacky = tackyProgram {
            function("main") {
                assign("x", 1.t * 2.t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-4),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                                ),
                                Assembly.Instruction.Binary(
                                    operator = Assembly.BinaryOperator.Mul,
                                    src = Assembly.Operand.Immediate(2),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.R11),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    class IdivInstructionTestCases : VarargArgumentsProvider(
        Tacky.BinaryOperator.Divide to Assembly.RegisterValue.Ax,
        Tacky.BinaryOperator.Modulo to Assembly.RegisterValue.Dx,
    )

    @ParameterizedTest
    @ArgumentsSource(IdivInstructionTestCases::class)
    fun `should generate idiv related insructions`(
        tackyOperator: Tacky.BinaryOperator,
        register: Assembly.RegisterValue,
    ) {
        val tacky = tackyProgram {
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.Divide -> 1.t / "y".t
                        Tacky.BinaryOperator.Modulo -> 1.t % "y".t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Cdq,
                                Assembly.Instruction.Idiv(
                                    operand = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(register),
                                    dst = Assembly.Operand.Stack(-8),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    class ConditionalTestCases : VarargArgumentsProvider(
        Tacky.BinaryOperator.LessThan to Assembly.ConditionalOperator.LessThan,
        Tacky.BinaryOperator.LessThanOrEqual to Assembly.ConditionalOperator.LessThanOrEqual,
        Tacky.BinaryOperator.GreaterThan to Assembly.ConditionalOperator.GreaterThan,
        Tacky.BinaryOperator.GreaterThanOrEqual to Assembly.ConditionalOperator.GreaterThanOrEqual,
        Tacky.BinaryOperator.Equal to Assembly.ConditionalOperator.Equal,
        Tacky.BinaryOperator.NotEqual to Assembly.ConditionalOperator.NotEqual,
    )

    @ParameterizedTest
    @ArgumentsSource(ConditionalTestCases::class)
    fun `should generate conditional instructions`(
        tackyOperator: Tacky.BinaryOperator,
        assemblyOperator: Assembly.ConditionalOperator,
    ) {
        val tacky = tackyProgram {
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.LessThan -> 1.t lt 2.t
                        Tacky.BinaryOperator.LessThanOrEqual -> 1.t lte 2.t
                        Tacky.BinaryOperator.GreaterThan -> 1.t gt 2.t
                        Tacky.BinaryOperator.GreaterThanOrEqual -> 1.t gte 2.t
                        Tacky.BinaryOperator.Equal -> 1.t eq 2.t
                        Tacky.BinaryOperator.NotEqual -> 1.t neq 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(0),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                // should not have cmp with immediate operand as dst
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Cmp(
                                    src = Assembly.Operand.Immediate(2),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Set(
                                    operator = assemblyOperator,
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    class ShiftTestCases : VarargArgumentsProvider(
        Tacky.BinaryOperator.ShiftLeft to Assembly.ShiftOperator.Left,
        Tacky.BinaryOperator.ShiftRight to Assembly.ShiftOperator.Right,
    )

    @ParameterizedTest
    @ArgumentsSource(ShiftTestCases::class)
    fun `should generate shift instructions`(
        tackyOperator: Tacky.BinaryOperator,
        assemblyOperator: Assembly.ShiftOperator,
    ) {
        val tacky = tackyProgram {
            function("main") {
                assign(
                    "x",
                    when (tackyOperator) {
                        Tacky.BinaryOperator.ShiftLeft -> 1.t shl 2.t
                        Tacky.BinaryOperator.ShiftRight -> 1.t shr 2.t
                        else -> error("Not in this test")
                    },
                )
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(2),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Cx),
                                ),
                                Assembly.Instruction.Shift(
                                    operator = assemblyOperator,
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate label instruction`() {
        val tacky = tackyProgram {
            function("main") {
                label("start")
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Label(
                                    label = LabelName("start"),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump if not zero`() {
        val tacky = tackyProgram {
            function("main") {
                jumpIfNotZero(42.t, "start")
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(0),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Cmp(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.ConditionalJump(
                                    operator = Assembly.ConditionalOperator.NotEqual,
                                    target = LabelName("start"),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump if zero`() {
        val tacky = tackyProgram {
            function("main") {
                jumpIfZero(42.t, "start")
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(0),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Cmp(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.ConditionalJump(
                                    operator = Assembly.ConditionalOperator.Equal,
                                    target = LabelName("start"),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump`() {
        val tacky = tackyProgram {
            function("main") {
                jump("start")
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Jump(
                                    target = LabelName("start"),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should not have mov with two stack offsets`() {
        val tacky = tackyProgram {
            function("main") {
                assign("y", "x".t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-4),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                    dst = Assembly.Operand.Stack(-8),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should not have cmp with two stack offsets`() {
        val tacky = tackyProgram {
            function("main") {
                assign("z", "x".t lt "y".t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(0),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-8),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Cmp(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                    dst = Assembly.Operand.Stack(-12),
                                ),
                                Assembly.Instruction.Set(
                                    operator = Assembly.ConditionalOperator.LessThan,
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should not have immediate operand as operand of idiv`() {
        val tacky = tackyProgram {
            function("main") {
                assign("x", 1.t / 2.t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(1),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Cdq,
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(2),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Idiv(
                                    operand = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                    dst = Assembly.Operand.Stack(-4),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should not have binary operators with two stack offsets`() {
        val tacky = tackyProgram {
            function("main") {
                assign("z", "x".t + "y".t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "main",
                            global = true,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(16),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-4),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                    dst = Assembly.Operand.Stack(-8),
                                ),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Stack(-12),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                ),
                                Assembly.Instruction.Binary(
                                    operator = Assembly.BinaryOperator.Add,
                                    src = Assembly.Operand.Register(Assembly.RegisterValue.R10),
                                    dst = Assembly.Operand.Stack(-8),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }

    @Test
    fun `should copy global attribute for functions`() {
        val tacky = tackyProgram {
            nonGlobalFunction("foo") {
                return_(42.t)
            }
        }

        val assembly = tackyProgramToAssembly(tacky, empty())

        assertEquals(
            Assembly.Program(
                topLevel =
                listOf(
                    Assembly.TopLevel.FunctionDefinition(
                        Assembly.FunctionDefinition(
                            name = "foo",
                            global = false,
                            body =
                            listOf(
                                Assembly.Instruction.AllocateStack(0),
                                Assembly.Instruction.Mov(
                                    src = Assembly.Operand.Immediate(42),
                                    dst = Assembly.Operand.Register(Assembly.RegisterValue.Ax),
                                ),
                                Assembly.Instruction.Ret,
                            ),
                        ),
                    ),
                ),
            ),
            assembly,
        )
    }
}
