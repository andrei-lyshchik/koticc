package koticc

import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class ASTToTackyKtTest {
    @Test
    fun `should produce tacky for a program with single return`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Return(
                                                AST.Expression.IntLiteral(1, Location(1, 0)),
                                                Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Return(
                                    value = Tacky.Value.IntConstant(1),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    class UnaryOperatorTestCases : VarargArgumentsProvider(
        AST.UnaryOperator.Negate to Tacky.UnaryOperator.Negate,
        AST.UnaryOperator.Complement to Tacky.UnaryOperator.Complement,
        AST.UnaryOperator.LogicalNegate to Tacky.UnaryOperator.LogicalNegate,
    )

    @ParameterizedTest
    @ArgumentsSource(UnaryOperatorTestCases::class)
    fun `should generate tacky for unary expression`(
        astUnaryOperator: AST.UnaryOperator,
        tackyUnaryOperator: Tacky.UnaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Unary(
                                                    operator = astUnaryOperator,
                                                    operand = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                    location = Location(1, 0),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Unary(
                                    operator = tackyUnaryOperator,
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    class NonShortCircuitingBinaryExpressionTestCases : VarargArgumentsProvider(
        AST.BinaryOperator.Add to Tacky.BinaryOperator.Add,
        AST.BinaryOperator.Subtract to Tacky.BinaryOperator.Subtract,
        AST.BinaryOperator.Multiply to Tacky.BinaryOperator.Multiply,
        AST.BinaryOperator.Divide to Tacky.BinaryOperator.Divide,
        AST.BinaryOperator.Modulo to Tacky.BinaryOperator.Modulo,
        AST.BinaryOperator.LessThan to Tacky.BinaryOperator.LessThan,
        AST.BinaryOperator.LessThanOrEqual to Tacky.BinaryOperator.LessThanOrEqual,
        AST.BinaryOperator.Equal to Tacky.BinaryOperator.Equal,
        AST.BinaryOperator.NotEqual to Tacky.BinaryOperator.NotEqual,
        AST.BinaryOperator.GreaterThan to Tacky.BinaryOperator.GreaterThan,
        AST.BinaryOperator.GreaterThanOrEqual to Tacky.BinaryOperator.GreaterThanOrEqual,
        AST.BinaryOperator.BitwiseAnd to Tacky.BinaryOperator.BitwiseAnd,
        AST.BinaryOperator.BitwiseOr to Tacky.BinaryOperator.BitwiseOr,
        AST.BinaryOperator.BitwiseXor to Tacky.BinaryOperator.BitwiseXor,
        AST.BinaryOperator.ShiftLeft to Tacky.BinaryOperator.ShiftLeft,
        AST.BinaryOperator.ShiftRight to Tacky.BinaryOperator.ShiftRight,
    )

    @ParameterizedTest
    @ArgumentsSource(NonShortCircuitingBinaryExpressionTestCases::class)
    fun `should generate tacky for non-short-circuiting binary expression`(
        astBinaryOperator: AST.BinaryOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Binary(
                                                    operator = astBinaryOperator,
                                                    left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                    right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should generate tacky for complex non-short-circuiting expression`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Binary(
                                                    operator = AST.BinaryOperator.Add,
                                                    left =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Multiply,
                                                            left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                        ),
                                                    right =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Divide,
                                                            left = AST.Expression.IntLiteral(3, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(4, Location(1, 0)),
                                                        ),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Multiply,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Divide,
                                    left = Tacky.Value.IntConstant(3),
                                    right = Tacky.Value.IntConstant(4),
                                    dst = Tacky.Value.Variable("tmp.1"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.Variable("tmp.0"),
                                    right = Tacky.Value.Variable("tmp.1"),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should generate tacky for logical and`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Binary(
                                                    operator = AST.BinaryOperator.LogicalAnd,
                                                    left =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Equal,
                                                            left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                        ),
                                                    right =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.NotEqual,
                                                            left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(3, Location(1, 0)),
                                                        ),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Equal,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.JumpIfZero(
                                    src = Tacky.Value.Variable("tmp.0"),
                                    target = LabelName("and_false.0"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.NotEqual,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(3),
                                    dst = Tacky.Value.Variable("tmp.1"),
                                ),
                                Tacky.Instruction.JumpIfZero(
                                    src = Tacky.Value.Variable("tmp.1"),
                                    target = LabelName("and_false.0"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Jump(LabelName("and_end.1")),
                                Tacky.Instruction.Label(LabelName("and_false.0")),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(0),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Label(LabelName("and_end.1")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should generate tacky for logical or`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Binary(
                                                    operator = AST.BinaryOperator.LogicalOr,
                                                    left =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Equal,
                                                            left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                        ),
                                                    right =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.NotEqual,
                                                            left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(3, Location(1, 0)),
                                                        ),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 0,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Equal,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.JumpIfNotZero(
                                    src = Tacky.Value.Variable("tmp.0"),
                                    target = LabelName("or_true.0"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.NotEqual,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(3),
                                    dst = Tacky.Value.Variable("tmp.1"),
                                ),
                                Tacky.Instruction.JumpIfNotZero(
                                    src = Tacky.Value.Variable("tmp.1"),
                                    target = LabelName("or_true.0"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(0),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Jump(LabelName("or_end.1")),
                                Tacky.Instruction.Label(LabelName("or_true.0")),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Label(LabelName("or_end.1")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should produce tacky for declaration with initializer`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 1,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should skip declaration without initializer`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = null,
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 1,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should start temp variable numbering from variable count`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer =
                                                    AST.Expression.Binary(
                                                        operator = AST.BinaryOperator.Add,
                                                        left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                        right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                    ),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 123,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.123"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("tmp.123"),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should produce tacky for assignment`() {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = null,
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.Assignment(
                                                    left = AST.Expression.Variable("x", Location(1, 0)),
                                                    right = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 1,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    class CompoundAssignmentTestCases : VarargArgumentsProvider(
        AST.CompoundAssignmentOperator.Add to Tacky.BinaryOperator.Add,
        AST.CompoundAssignmentOperator.Subtract to Tacky.BinaryOperator.Subtract,
        AST.CompoundAssignmentOperator.Multiply to Tacky.BinaryOperator.Multiply,
        AST.CompoundAssignmentOperator.Divide to Tacky.BinaryOperator.Divide,
        AST.CompoundAssignmentOperator.Modulo to Tacky.BinaryOperator.Modulo,
        AST.CompoundAssignmentOperator.BitwiseAnd to Tacky.BinaryOperator.BitwiseAnd,
        AST.CompoundAssignmentOperator.BitwiseOr to Tacky.BinaryOperator.BitwiseOr,
        AST.CompoundAssignmentOperator.BitwiseXor to Tacky.BinaryOperator.BitwiseXor,
        AST.CompoundAssignmentOperator.ShiftLeft to Tacky.BinaryOperator.ShiftLeft,
        AST.CompoundAssignmentOperator.ShiftRight to Tacky.BinaryOperator.ShiftRight,
    )

    @ParameterizedTest
    @ArgumentsSource(CompoundAssignmentTestCases::class)
    fun `should produce tacky for compound assignment`(
        astCompoundAssignmentOperator: AST.CompoundAssignmentOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.Expression(
                                                AST.Expression.CompoundAssignment(
                                                    operator = astCompoundAssignmentOperator,
                                                    left = AST.Expression.Variable("x", Location(1, 0)),
                                                    right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                ),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 1,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.1"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("tmp.1"),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    class PostfixTestCases : VarargArgumentsProvider(
        AST.PostfixOperator.Increment to Tacky.BinaryOperator.Add,
        AST.PostfixOperator.Decrement to Tacky.BinaryOperator.Subtract,
    )

    @ParameterizedTest
    @ArgumentsSource(PostfixTestCases::class)
    fun `should produce tacky for postfix operator`(
        postfixOperator: AST.PostfixOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "y",
                                                initializer =
                                                    AST.Expression.Binary(
                                                        operator = AST.BinaryOperator.Add,
                                                        left =
                                                            AST.Expression.Postfix(
                                                                operator = postfixOperator,
                                                                operand = AST.Expression.Variable("x", Location(1, 0)),
                                                            ),
                                                        right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                    ),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.Return(
                                                expression =
                                                    AST.Expression.Binary(
                                                        operator = AST.BinaryOperator.Add,
                                                        left = AST.Expression.Variable("x", Location(1, 0)),
                                                        right = AST.Expression.Variable("y", Location(1, 0)),
                                                    ),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 2,
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("x"),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.Variable("tmp.2"),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.3"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("tmp.3"),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.Variable("y"),
                                    dst = Tacky.Value.Variable("tmp.4"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.Variable("tmp.4")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should produce tacky for if statement without else`() {
        val input =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "y",
                                                initializer = AST.Expression.IntLiteral(0, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.If(
                                                condition =
                                                    AST.Expression.Binary(
                                                        operator = AST.BinaryOperator.Equal,
                                                        left = AST.Expression.Variable("x", Location(1, 0)),
                                                        right = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                    ),
                                                thenStatement =
                                                    AST.Statement.Expression(
                                                        AST.Expression.Assignment(
                                                            left = AST.Expression.Variable("y", Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                        ),
                                                    ),
                                                elseStatement = null,
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.Return(
                                                expression = AST.Expression.Variable("y", Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 2,
            )

        val tacky = programASTToTacky(input)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(0),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Equal,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.JumpIfZero(
                                    src = Tacky.Value.Variable("tmp.2"),
                                    target = LabelName("if_else.0"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Label(LabelName("if_else.0")),
                                Tacky.Instruction.Return(Tacky.Value.Variable("y")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }

    @Test
    fun `should produce tacky for if statement with else`() {
        val input =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "x",
                                                initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            AST.Declaration(
                                                name = "y",
                                                initializer = AST.Expression.IntLiteral(0, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.If(
                                                condition =
                                                    AST.Expression.Binary(
                                                        operator = AST.BinaryOperator.Equal,
                                                        left = AST.Expression.Variable("x", Location(1, 0)),
                                                        right = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                    ),
                                                thenStatement =
                                                    AST.Statement.Expression(
                                                        AST.Expression.Assignment(
                                                            left = AST.Expression.Variable("y", Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                                        ),
                                                    ),
                                                elseStatement =
                                                    AST.Statement.Expression(
                                                        AST.Expression.Assignment(
                                                            left = AST.Expression.Variable("y", Location(1, 0)),
                                                            right = AST.Expression.IntLiteral(3, Location(1, 0)),
                                                        ),
                                                    ),
                                            ),
                                        ),
                                        AST.BlockItem.Statement(
                                            AST.Statement.Return(
                                                expression = AST.Expression.Variable("y", Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                location = Location(0, 0),
                            ),
                    ),
                variableCount = 2,
            )

        val tacky = programASTToTacky(input)

        assertEquals(
            Tacky.Program(
                functionDefinition =
                    Tacky.FunctionDefinition(
                        name = "main",
                        body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(0),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Equal,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.JumpIfZero(
                                    src = Tacky.Value.Variable("tmp.2"),
                                    target = LabelName("if_else.0"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Jump(
                                    target = LabelName("if_end.1"),
                                ),
                                Tacky.Instruction.Label(LabelName("if_else.0")),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(3),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Label(LabelName("if_end.1")),
                                Tacky.Instruction.Return(Tacky.Value.Variable("y")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                    ),
            ),
            tacky,
        )
    }
}
