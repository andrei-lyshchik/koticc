package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.e
import koticc.ast.program
import koticc.token.Location
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.EnumSource

class SemanticAnalysisKtTest {
    @Test
    fun `should replace variables name with unique ones and return count`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer =
                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer =
                                        AST.Expression.IntLiteral(2, Location(3, 13)),
                                        storageClass = null,
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer =
                                            AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer =
                                            AST.Expression.IntLiteral(2, Location(3, 13)),
                                            storageClass = null,
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
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "b.1" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should return error when variable is not declared`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
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
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer =
                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer =
                                        AST.Expression.IntLiteral(2, Location(3, 13)),
                                        storageClass = null,
                                        location = Location(3, 5),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            SemanticAnalysisError("'a' already declared at line 2, column 5", Location(3, 5))

        assertEquals(expected.left(), semanticAnalysis(input))
    }

    @Test
    fun `should return error when left side of assignment is not a variable`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
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
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
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
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
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
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
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
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should replace variable names with new names in declaration initializers`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer = AST.Expression.IntLiteral(2, Location(3, 13)),
                                        storageClass = null,
                                        location = Location(3, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
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
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer = AST.Expression.IntLiteral(2, Location(3, 13)),
                                            storageClass = null,
                                            location = Location(3, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
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
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 3,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "b.1" to Type.Integer.toIdentifier(),
                    "c.2" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should support case with variable used in its own initializer`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
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
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
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
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should handle if statement`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer =
                                        AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer =
                                            AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
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
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should handle conditional expression`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer = null,
                                        storageClass = null,
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
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer = null,
                                            storageClass = null,
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
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "b.1" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should allow declaring same variable in nested scope`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Statement(
                                    statement =
                                    AST.Statement.If(
                                        condition = AST.Expression.IntLiteral(1, Location(3, 5)),
                                        thenStatement =
                                        AST.Statement.Compound(
                                            AST.Block(
                                                blockItems =
                                                listOf(
                                                    AST.BlockItem.Declaration(
                                                        declaration =
                                                        AST.Declaration.Variable(
                                                            name = "a",
                                                            initializer =
                                                            AST.Expression.IntLiteral(
                                                                2,
                                                                Location(4, 13),
                                                            ),
                                                            storageClass = null,
                                                            location = Location(4, 5),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                        elseStatement = AST.Statement.Null(Location(5, 1)),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Statement(
                                        statement =
                                        AST.Statement.If(
                                            condition = AST.Expression.IntLiteral(1, Location(3, 5)),
                                            thenStatement =
                                            AST.Statement.Compound(
                                                AST.Block(
                                                    blockItems =
                                                    listOf(
                                                        AST.BlockItem.Declaration(
                                                            declaration =
                                                            AST.Declaration.Variable(
                                                                name = "a.1",
                                                                initializer =
                                                                AST.Expression.IntLiteral(
                                                                    2,
                                                                    Location(4, 13),
                                                                ),
                                                                storageClass = null,
                                                                location = Location(4, 5),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                            elseStatement = AST.Statement.Null(Location(5, 1)),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                    "a.1" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should allow using variable from outer scope`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Statement(
                                    statement =
                                    AST.Statement.If(
                                        condition = AST.Expression.IntLiteral(1, Location(3, 5)),
                                        thenStatement =
                                        AST.Statement.Compound(
                                            AST.Block(
                                                blockItems =
                                                listOf(
                                                    AST.BlockItem.Statement(
                                                        statement =
                                                        AST.Statement.Expression(
                                                            expression =
                                                            AST.Expression.Assignment(
                                                                left =
                                                                AST.Expression.Variable(
                                                                    "a",
                                                                    Location(4, 5),
                                                                ),
                                                                right =
                                                                AST.Expression.IntLiteral(
                                                                    2,
                                                                    Location(4, 9),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                        elseStatement = AST.Statement.Null(Location(5, 1)),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "a.0",
                                            initializer = AST.Expression.IntLiteral(
                                                1,
                                                Location(2, 13),
                                            ),
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Statement(
                                        statement =
                                        AST.Statement.If(
                                            condition = AST.Expression.IntLiteral(1, Location(3, 5)),
                                            thenStatement =
                                            AST.Statement.Compound(
                                                AST.Block(
                                                    blockItems =
                                                    listOf(
                                                        AST.BlockItem.Statement(
                                                            statement =
                                                            AST.Statement.Expression(
                                                                expression =
                                                                AST.Expression.Assignment(
                                                                    left =
                                                                    AST.Expression.Variable(
                                                                        "a.0",
                                                                        Location(4, 5),
                                                                    ),
                                                                    right =
                                                                    AST.Expression.IntLiteral(
                                                                        2,
                                                                        Location(4, 9),
                                                                    ),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                            elseStatement = AST.Statement.Null(Location(5, 1)),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "a.0" to Type.Integer.toIdentifier(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should allow first using and then shadowing variable from outer scope`() {
        val input =
            AST.Program(
                declarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),

                                AST.BlockItem.Statement(
                                    statement =
                                    AST.Statement.Compound(
                                        AST.Block(
                                            blockItems =
                                            listOf(
                                                AST.BlockItem.Statement(
                                                    statement =
                                                    AST.Statement.Expression(
                                                        expression =
                                                        AST.Expression.Assignment(
                                                            left = AST.Expression.Variable(
                                                                "a",
                                                                Location(2, 5),
                                                            ),
                                                            right = AST.Expression.IntLiteral(
                                                                1,
                                                                Location(2, 9),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration =
                                                    AST.Declaration.Variable(
                                                        name = "a",
                                                        initializer = AST.Expression.IntLiteral(
                                                            2,
                                                            Location(3, 13),
                                                        ),
                                                        storageClass = null,
                                                        location = Location(3, 5),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected = ValidASTProgram(
            value = AST.Program(
                declarations = listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body = AST.Block(
                            blockItems = listOf(
                                AST.BlockItem.Declaration(
                                    declaration = AST.Declaration.Variable(
                                        name = "a.0",
                                        initializer = AST.Expression.IntLiteral(1, Location(2, 13)),
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Statement(
                                    statement = AST.Statement.Compound(
                                        AST.Block(
                                            blockItems = listOf(
                                                AST.BlockItem.Statement(
                                                    statement = AST.Statement.Expression(
                                                        expression = AST.Expression.Assignment(
                                                            left = AST.Expression.Variable("a.0", Location(2, 5)),
                                                            right = AST.Expression.IntLiteral(1, Location(2, 9)),
                                                        ),
                                                    ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    declaration = AST.Declaration.Variable(
                                                        name = "a.1",
                                                        initializer = AST.Expression.IntLiteral(2, Location(3, 13)),
                                                        storageClass = null,
                                                        location = Location(3, 5),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            ),
            renamedVariableCount = 2,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "a.0" to Type.Integer.toIdentifier(),
                "a.1" to Type.Integer.toIdentifier(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should not allow declaring variable multiple times in nested scope`() {
        val input = program {
            function("main") {
                int("a") assign 1.e
                nested {
                    int("a") assign 2.e
                    int("a") assign 2.e
                }
            }
        }

        val expected = SemanticAnalysisError("'a' already declared at line 0, column 0", Location(0, 0))

        assertEquals(expected.left(), semanticAnalysis(input))
    }
}
