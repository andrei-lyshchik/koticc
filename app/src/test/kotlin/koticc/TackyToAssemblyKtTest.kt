package koticc

import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class TackyToAssemblyKtTest {
    @Test
    fun `should generate assembly for single return`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Return(
                                value = Tacky.Value.IntConstant(42),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should allocate stack for variables`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.IntConstant(42),
                                dst = Tacky.Value.Variable("x"),
                            ),
                            Tacky.Instruction.Return(
                                value = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Unary(
                                operator = tackyOperator,
                                src = Tacky.Value.IntConstant(42),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate assembly for logical negate`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Unary(
                                operator = Tacky.UnaryOperator.LogicalNegate,
                                src = Tacky.Value.IntConstant(42),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = tackyOperator,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.IntConstant(2),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate assembly for multiplication`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = Tacky.BinaryOperator.Multiply,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.IntConstant(2),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = tackyOperator,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.Variable("y"),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = tackyOperator,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.IntConstant(2),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = tackyOperator,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.IntConstant(2),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate label instruction`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Label(
                                label = LabelName("start"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump if not zero`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.JumpIfNotZero(
                                src = Tacky.Value.IntConstant(42),
                                target = LabelName("start"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump if zero`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.JumpIfZero(
                                src = Tacky.Value.IntConstant(42),
                                target = LabelName("start"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should generate assembly for jump`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Jump(
                                target = LabelName("start"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should not have mov with two stack offsets`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.Variable("x"),
                                dst = Tacky.Value.Variable("y"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should not have cmp with two stack offsets`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = Tacky.BinaryOperator.LessThan,
                                left = Tacky.Value.Variable("x"),
                                right = Tacky.Value.Variable("y"),
                                dst = Tacky.Value.Variable("z"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should not have immediate operand as operand of idiv`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = Tacky.BinaryOperator.Divide,
                                left = Tacky.Value.IntConstant(1),
                                right = Tacky.Value.IntConstant(2),
                                dst = Tacky.Value.Variable("x"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }

    @Test
    fun `should not have binary operators with two stack offsets`() {
        val tacky =
            Tacky.Program(
                functionDefinitions =
                listOf(
                    Tacky.FunctionDefinition(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        listOf(
                            Tacky.Instruction.Binary(
                                operator = Tacky.BinaryOperator.Add,
                                left = Tacky.Value.Variable("x"),
                                right = Tacky.Value.Variable("y"),
                                dst = Tacky.Value.Variable("z"),
                            ),
                        ),
                    ),
                ),
            )

        val assembly = tackyProgramToAssembly(tacky)

        assertEquals(
            Assembly.Program(
                functionDefinitions =
                listOf(
                    Assembly.FunctionDefinition(
                        name = "main",
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
            assembly,
        )
    }
}
