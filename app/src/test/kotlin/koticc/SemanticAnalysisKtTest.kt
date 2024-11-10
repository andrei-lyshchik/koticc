package koticc

import arrow.core.left
import arrow.core.right
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.EnumSource

class SemanticAnalysisKtTest {
    @Test
    fun `should replace variables name with unique ones and return count`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer =
                                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "b",
                                                    initializer =
                                                        AST.Expression.IntLiteral(2, Location(3, 13)),
                                                    location = Location(3, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Return(
                                                    expression =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Add,
                                                            left =
                                                                AST.Expression.Variable(
                                                                    "a",
                                                                    Location(4, 12),
                                                                ),
                                                            right =
                                                                AST.Expression.Variable(
                                                                    "b",
                                                                    Location(4, 16),
                                                                ),
                                                        ),
                                                    location = Location(4, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer =
                                                                AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "b.1",
                                                            initializer =
                                                                AST.Expression.IntLiteral(2, Location(3, 13)),
                                                            location = Location(3, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Statement(
                                                    statement =
                                                        AST.Statement.Return(
                                                            expression =
                                                                AST.Expression.Binary(
                                                                    operator = AST.BinaryOperator.Add,
                                                                    left =
                                                                        AST.Expression.Variable(
                                                                            "a.0",
                                                                            Location(4, 12),
                                                                        ),
                                                                    right =
                                                                        AST.Expression.Variable(
                                                                            "b.1",
                                                                            Location(4, 16),
                                                                        ),
                                                                ),
                                                            location = Location(4, 5),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 2,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should return error when variable is not declared`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.Variable(
                                                            "a",
                                                            Location(2, 5),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            SemanticAnalysisError("undeclared variable 'a'", Location(2, 5))

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should return error when variable declared multiple times`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer =
                                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer =
                                                        AST.Expression.IntLiteral(2, Location(3, 13)),
                                                    location = Location(3, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            SemanticAnalysisError("variable 'a' already declared at line 2, column 5", Location(3, 5))

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should return error when left side of assignment is not a variable`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.Assignment(
                                                            left =
                                                                AST.Expression.IntLiteral(1, Location(2, 5)),
                                                            right =
                                                                AST.Expression.IntLiteral(2, Location(2, 9)),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            SemanticAnalysisError(
                "left side of assignment must be a left-value, got IntLiteral(value=1, location=Location(line=2, column=5))",
                Location(2, 5),
            )

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @ParameterizedTest
    @EnumSource(AST.CompoundAssignmentOperator::class)
    fun `should return error if left side of compound assignment is not a variable`(
        compoundAssignmentOperator: AST.CompoundAssignmentOperator,
    ) {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.CompoundAssignment(
                                                            operator = compoundAssignmentOperator,
                                                            left =
                                                                AST.Expression.IntLiteral(1, Location(2, 5)),
                                                            right =
                                                                AST.Expression.IntLiteral(2, Location(2, 9)),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            SemanticAnalysisError(
                "left side of compound assignment must be a left-value, got IntLiteral(value=1, location=Location(line=2, column=5))",
                Location(2, 5),
            )

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @ParameterizedTest
    @EnumSource(AST.PostfixOperator::class)
    fun `should return error if left side of postfix operator is not a variable`(postfixOperator: AST.PostfixOperator) {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.Postfix(
                                                            operator = postfixOperator,
                                                            operand = AST.Expression.IntLiteral(1, Location(2, 5)),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            SemanticAnalysisError(
                "operand of postfix operator must be a left-value, got IntLiteral(value=1, location=Location(line=2, column=5))",
                Location(2, 5),
            )

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @ParameterizedTest
    @EnumSource(AST.PostfixOperator::class)
    fun `should replace variable names in postfix operator operand`(postfixOperator: AST.PostfixOperator) {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.Postfix(
                                                            operator = postfixOperator,
                                                            operand = AST.Expression.Variable("a", Location(3, 5)),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Statement(
                                                    statement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                                AST.Expression.Postfix(
                                                                    operator = postfixOperator,
                                                                    operand = AST.Expression.Variable("a.0", Location(3, 5)),
                                                                ),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 1,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should replace variable names with new names in declaration initializers`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "b",
                                                    initializer = AST.Expression.IntLiteral(2, Location(3, 13)),
                                                    location = Location(3, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "c",
                                                    initializer =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Add,
                                                            left =
                                                                AST.Expression.Variable(
                                                                    "a",
                                                                    Location(2, 13),
                                                                ),
                                                            right =
                                                                AST.Expression.Variable(
                                                                    "b",
                                                                    Location(2, 17),
                                                                ),
                                                        ),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "b.1",
                                                            initializer = AST.Expression.IntLiteral(2, Location(3, 13)),
                                                            location = Location(3, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "c.2",
                                                            initializer =
                                                                AST.Expression.Binary(
                                                                    operator = AST.BinaryOperator.Add,
                                                                    left =
                                                                        AST.Expression.Variable(
                                                                            "a.0",
                                                                            Location(2, 13),
                                                                        ),
                                                                    right =
                                                                        AST.Expression.Variable(
                                                                            "b.1",
                                                                            Location(2, 17),
                                                                        ),
                                                                ),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 3,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should support case with variable used in its own initializer`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Add,
                                                            left =
                                                                AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            right =
                                                                AST.Expression.Variable(
                                                                    "a",
                                                                    Location(2, 17),
                                                                ),
                                                        ),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer =
                                                                AST.Expression.Binary(
                                                                    operator = AST.BinaryOperator.Add,
                                                                    left =
                                                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                                                    right =
                                                                        AST.Expression.Variable(
                                                                            "a.0",
                                                                            Location(2, 17),
                                                                        ),
                                                                ),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 1,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should handle if statement`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer =
                                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.If(
                                                    condition =
                                                        AST.Expression.Binary(
                                                            operator = AST.BinaryOperator.Equal,
                                                            left =
                                                                AST.Expression.Variable(
                                                                    "a",
                                                                    Location(3, 13),
                                                                ),
                                                            right =
                                                                AST.Expression.IntLiteral(1, Location(3, 17)),
                                                        ),
                                                    thenStatement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                                AST.Expression.Assignment(
                                                                    left =
                                                                        AST.Expression.Variable(
                                                                            "a",
                                                                            Location(4, 5),
                                                                        ),
                                                                    right =
                                                                        AST.Expression.IntLiteral(2, Location(4, 9)),
                                                                ),
                                                        ),
                                                    elseStatement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                                AST.Expression.Assignment(
                                                                    left =
                                                                        AST.Expression.Variable(
                                                                            "a",
                                                                            Location(5, 5),
                                                                        ),
                                                                    right =
                                                                        AST.Expression.IntLiteral(3, Location(5, 9)),
                                                                ),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer =
                                                                AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Statement(
                                                    statement =
                                                        AST.Statement.If(
                                                            condition =
                                                                AST.Expression.Binary(
                                                                    operator = AST.BinaryOperator.Equal,
                                                                    left =
                                                                        AST.Expression.Variable(
                                                                            "a.0",
                                                                            Location(3, 13),
                                                                        ),
                                                                    right =
                                                                        AST.Expression.IntLiteral(1, Location(3, 17)),
                                                                ),
                                                            thenStatement =
                                                                AST.Statement.Expression(
                                                                    expression =
                                                                        AST.Expression.Assignment(
                                                                            left =
                                                                                AST.Expression.Variable(
                                                                                    "a.0",
                                                                                    Location(4, 5),
                                                                                ),
                                                                            right =
                                                                                AST.Expression.IntLiteral(2, Location(4, 9)),
                                                                        ),
                                                                ),
                                                            elseStatement =
                                                                AST.Statement.Expression(
                                                                    expression =
                                                                        AST.Expression.Assignment(
                                                                            left =
                                                                                AST.Expression.Variable(
                                                                                    "a.0",
                                                                                    Location(5, 5),
                                                                                ),
                                                                            right =
                                                                                AST.Expression.IntLiteral(3, Location(5, 9)),
                                                                        ),
                                                                ),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 1,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should handle conditional expression`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "b",
                                                    initializer = null,
                                                    location = Location(3, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Expression(
                                                    expression =
                                                        AST.Expression.Conditional(
                                                            condition =
                                                                AST.Expression.Binary(
                                                                    operator = AST.BinaryOperator.Equal,
                                                                    left = AST.Expression.Variable("a", Location(3, 13)),
                                                                    right = AST.Expression.IntLiteral(1, Location(3, 17)),
                                                                ),
                                                            thenExpression =
                                                                AST.Expression.Assignment(
                                                                    left = AST.Expression.Variable("b", Location(4, 5)),
                                                                    right = AST.Expression.IntLiteral(2, Location(4, 9)),
                                                                ),
                                                            elseExpression =
                                                                AST.Expression.Assignment(
                                                                    left = AST.Expression.Variable("b", Location(5, 5)),
                                                                    right = AST.Expression.IntLiteral(3, Location(5, 9)),
                                                                ),
                                                        ),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "b.1",
                                                            initializer = null,
                                                            location = Location(3, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Statement(
                                                    statement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                                AST.Expression.Conditional(
                                                                    condition =
                                                                        AST.Expression.Binary(
                                                                            operator = AST.BinaryOperator.Equal,
                                                                            left = AST.Expression.Variable("a.0", Location(3, 13)),
                                                                            right = AST.Expression.IntLiteral(1, Location(3, 17)),
                                                                        ),
                                                                    thenExpression =
                                                                        AST.Expression.Assignment(
                                                                            left = AST.Expression.Variable("b.1", Location(4, 5)),
                                                                            right = AST.Expression.IntLiteral(2, Location(4, 9)),
                                                                        ),
                                                                    elseExpression =
                                                                        AST.Expression.Assignment(
                                                                            left = AST.Expression.Variable("b.1", Location(5, 5)),
                                                                            right = AST.Expression.IntLiteral(3, Location(5, 9)),
                                                                        ),
                                                                ),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 2,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should handle labeled statement`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Declaration(
                                            declaration =
                                                AST.Declaration(
                                                    name = "a",
                                                    initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Labeled(
                                                    label = LabelName("label"),
                                                    statement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                                AST.Expression.Assignment(
                                                                    left = AST.Expression.Variable("a", Location(3, 5)),
                                                                    right = AST.Expression.IntLiteral(2, Location(3, 9)),
                                                                ),
                                                        ),
                                                    location = Location(3, 1),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value =
                    AST.Program(
                        functionDefinition =
                            AST.FunctionDefinition(
                                name = "main",
                                body =
                                    AST.Block(
                                        blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                        AST.Declaration(
                                                            name = "a.0",
                                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                                            location = Location(2, 5),
                                                        ),
                                                ),
                                                AST.BlockItem.Statement(
                                                    statement =
                                                        AST.Statement.Labeled(
                                                            label = LabelName("label"),
                                                            statement =
                                                                AST.Statement.Expression(
                                                                    expression =
                                                                        AST.Expression.Assignment(
                                                                            left = AST.Expression.Variable("a.0", Location(3, 5)),
                                                                            right = AST.Expression.IntLiteral(2, Location(3, 9)),
                                                                        ),
                                                                ),
                                                            location = Location(3, 1),
                                                        ),
                                                ),
                                            ),
                                    ),
                                location = Location(1, 1),
                            ),
                    ),
                variableCount = 1,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should return error if goto label is not declared`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Goto(
                                                    label = LabelName("label"),
                                                    location = Location(2, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected = SemanticAnalysisError("goto to undeclared label 'label'", Location(2, 5))

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should return error if labels on labeled statements are not unique`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Labeled(
                                                    label = LabelName("label"),
                                                    statement = AST.Statement.Null(Location(2, 1)),
                                                    location = Location(2, 1),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Labeled(
                                                    label = LabelName("label"),
                                                    statement = AST.Statement.Null(Location(3, 1)),
                                                    location = Location(3, 1),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected = SemanticAnalysisError("label 'label' already declared at line 2, column 1", Location(3, 1))

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should consider labels inside if else`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.If(
                                                    condition = AST.Expression.IntLiteral(1, Location(2, 5)),
                                                    thenStatement =
                                                        AST.Statement.Labeled(
                                                            label = LabelName("label_if"),
                                                            statement = AST.Statement.Null(Location(3, 1)),
                                                            location = Location(3, 1),
                                                        ),
                                                    elseStatement =
                                                        AST.Statement.Labeled(
                                                            label = LabelName("label_else"),
                                                            statement = AST.Statement.Null(Location(4, 1)),
                                                            location = Location(4, 1),
                                                        ),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Goto(
                                                    label = LabelName("label_if"),
                                                    location = Location(5, 5),
                                                ),
                                        ),
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.Goto(
                                                    label = LabelName("label_else"),
                                                    location = Location(6, 5),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected =
            ValidASTProgram(
                value = input,
                variableCount = 0,
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should consider non top level gotos`() {
        val input =
            AST.Program(
                functionDefinition =
                    AST.FunctionDefinition(
                        name = "main",
                        body =
                            AST.Block(
                                blockItems =
                                    listOf(
                                        AST.BlockItem.Statement(
                                            statement =
                                                AST.Statement.If(
                                                    condition = AST.Expression.IntLiteral(1, Location(3, 5)),
                                                    thenStatement =
                                                        AST.Statement.Goto(
                                                            label = LabelName("non_existing_label"),
                                                            location = Location(4, 5),
                                                        ),
                                                    elseStatement = AST.Statement.Null(Location(5, 1)),
                                                ),
                                        ),
                                    ),
                            ),
                        location = Location(1, 1),
                    ),
            )

        val expected = SemanticAnalysisError("goto to undeclared label 'non_existing_label'", Location(4, 5))

        assertEquals(expected.left(), semanticAnalysis(input))
    }
}
