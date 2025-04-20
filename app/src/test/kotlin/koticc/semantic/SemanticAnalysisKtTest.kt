package koticc.semantic

import arrow.core.left
import arrow.core.right
import koticc.ast.AST
import koticc.ast.Type
import koticc.ast.cast
import koticc.ast.e
import koticc.ast.int
import koticc.ast.long
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
                                        AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer =
                                        AST.Expression.Constant(AST.IntConstant(2), null, Location(3, 13)),
                                        type = Type.Int,
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
                                                null,
                                                Location(4, 12),
                                            ),
                                            right =
                                            AST.Expression.Variable(
                                                "b",
                                                null,
                                                Location(4, 16),
                                            ),
                                            type = null,
                                        ),
                                        location = Location(4, 5),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer =
                                            AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(3, 13)),
                                            type = Type.Int,
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
                                                    Type.Int,
                                                    Location(4, 12),
                                                ),
                                                right =
                                                AST.Expression.Variable(
                                                    "b.1",
                                                    Type.Int,
                                                    Location(4, 16),
                                                ),
                                                type = Type.Int,
                                            ),
                                            location = Location(4, 5),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "b.1" to Type.Int.toSymbol(),
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
                                            null,
                                            Location(2, 5),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                        AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "a",
                                        initializer =
                                        AST.Expression.Constant(AST.IntConstant(2), null, Location(3, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(3, 5),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 5)),
                                            right =
                                            AST.Expression.Constant(AST.IntConstant(2), null, Location(2, 9)),
                                            type = null,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            SemanticAnalysisError(
                "left side of assignment must be a left-value, got '1'",
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
                                            operand = AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 5)),
                                            type = null,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                        storageClass = null,
                        location = Location(1, 1),
                    ),
                ),
            )

        val expected =
            SemanticAnalysisError(
                "operand of postfix expression must be a lvalue, got 1",
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
                                        initializer = AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
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
                                            operand = AST.Expression.Variable("a", null, Location(3, 5)),
                                            type = null,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            initializer = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
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
                                                operand = AST.Expression.Variable("a.0", Type.Int, Location(3, 5)),
                                                type = Type.Int,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
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
                                        initializer = AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer = AST.Expression.Constant(AST.IntConstant(2), null, Location(3, 13)),
                                        type = Type.Int,
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
                                                null,
                                                Location(2, 13),
                                            ),
                                            right =
                                            AST.Expression.Variable(
                                                "b",
                                                null,
                                                Location(2, 17),
                                            ),
                                            type = null,
                                        ),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            initializer = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer = AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(3, 13)),
                                            type = Type.Int,
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
                                                    Type.Int,
                                                    Location(2, 13),
                                                ),
                                                right =
                                                AST.Expression.Variable(
                                                    "b.1",
                                                    Type.Int,
                                                    Location(2, 17),
                                                ),
                                                type = Type.Int,
                                            ),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 3,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "b.1" to Type.Int.toSymbol(),
                    "c.2" to Type.Int.toSymbol(),
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
                                            AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                            right =
                                            AST.Expression.Variable(
                                                "a",
                                                null,
                                                Location(2, 17),
                                            ),
                                            type = null,
                                        ),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                                AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                                right =
                                                AST.Expression.Variable(
                                                    "a.0",
                                                    Type.Int,
                                                    Location(2, 17),
                                                ),
                                                type = Type.Int,
                                            ),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
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
                                        AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
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
                                                null,
                                                Location(3, 13),
                                            ),
                                            right =
                                            AST.Expression.Constant(AST.IntConstant(1), null, Location(3, 17)),
                                            type = null,
                                        ),
                                        thenStatement =
                                        AST.Statement.Expression(
                                            expression =
                                            AST.Expression.Assignment(
                                                left =
                                                AST.Expression.Variable(
                                                    "a",
                                                    null,
                                                    Location(4, 5),
                                                ),
                                                right =
                                                AST.Expression.Constant(AST.IntConstant(2), null, Location(4, 9)),
                                                type = null,
                                            ),
                                        ),
                                        elseStatement =
                                        AST.Statement.Expression(
                                            expression =
                                            AST.Expression.Assignment(
                                                left =
                                                AST.Expression.Variable(
                                                    "a",
                                                    null,
                                                    Location(5, 5),
                                                ),
                                                right =
                                                AST.Expression.Constant(AST.IntConstant(3), null, Location(5, 9)),
                                                type = null,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
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
                                                    Type.Int,
                                                    Location(3, 13),
                                                ),
                                                right =
                                                AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(3, 17)),
                                                type = Type.Int,
                                            ),
                                            thenStatement =
                                            AST.Statement.Expression(
                                                expression =
                                                AST.Expression.Assignment(
                                                    left =
                                                    AST.Expression.Variable(
                                                        "a.0",
                                                        Type.Int,
                                                        Location(4, 5),
                                                    ),
                                                    right =
                                                    AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(4, 9)),
                                                    type = Type.Int,
                                                ),
                                            ),
                                            elseStatement =
                                            AST.Statement.Expression(
                                                expression =
                                                AST.Expression.Assignment(
                                                    left =
                                                    AST.Expression.Variable(
                                                        "a.0",
                                                        Type.Int,
                                                        Location(5, 5),
                                                    ),
                                                    right =
                                                    AST.Expression.Constant(AST.IntConstant(3), Type.Int, Location(5, 9)),
                                                    type = Type.Int,
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
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
                                        initializer = AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Declaration(
                                    declaration =
                                    AST.Declaration.Variable(
                                        name = "b",
                                        initializer = null,
                                        type = Type.Int,
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
                                                left = AST.Expression.Variable("a", null, Location(3, 13)),
                                                right = AST.Expression.Constant(AST.IntConstant(1), null, Location(3, 17)),
                                                type = null,
                                            ),
                                            thenExpression =
                                            AST.Expression.Assignment(
                                                left = AST.Expression.Variable("b", null, Location(4, 5)),
                                                right = AST.Expression.Constant(AST.IntConstant(2), null, Location(4, 9)),
                                                type = null,
                                            ),
                                            elseExpression =
                                            AST.Expression.Assignment(
                                                left = AST.Expression.Variable("b", null, Location(5, 5)),
                                                right = AST.Expression.Constant(AST.IntConstant(3), null, Location(5, 9)),
                                                type = null,
                                            ),
                                            type = null,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            initializer = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        declaration =
                                        AST.Declaration.Variable(
                                            name = "b.1",
                                            initializer = null,
                                            type = Type.Int,
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
                                                    left = AST.Expression.Variable("a.0", Type.Int, Location(3, 13)),
                                                    right = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(3, 17)),
                                                    type = Type.Int,
                                                ),
                                                thenExpression =
                                                AST.Expression.Assignment(
                                                    left = AST.Expression.Variable("b.1", Type.Int, Location(4, 5)),
                                                    right = AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(4, 9)),
                                                    type = Type.Int,
                                                ),
                                                elseExpression =
                                                AST.Expression.Assignment(
                                                    left = AST.Expression.Variable("b.1", Type.Int, Location(5, 5)),
                                                    right = AST.Expression.Constant(AST.IntConstant(3), Type.Int, Location(5, 9)),
                                                    type = Type.Int,
                                                ),
                                                type = Type.Int,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "b.1" to Type.Int.toSymbol(),
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
                                        initializer = AST.Expression.Constant(AST.IntConstant(1), null, Location(2, 13)),
                                        type = Type.Int,
                                        storageClass = null,
                                        location = Location(2, 5),
                                    ),
                                ),
                                AST.BlockItem.Statement(
                                    statement =
                                    AST.Statement.If(
                                        condition = AST.Expression.Constant(AST.IntConstant(1), null, Location(3, 5)),
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
                                                            AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(4, 13)),
                                                            type = Type.Int,
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
                        type = Type.Function(parameters = emptyList(), returnType = Type.Int),
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
                                            initializer = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(2, 13)),
                                            type = Type.Int,
                                            storageClass = null,
                                            location = Location(2, 5),
                                        ),
                                    ),
                                    AST.BlockItem.Statement(
                                        statement =
                                        AST.Statement.If(
                                            condition = AST.Expression.Constant(AST.IntConstant(1), Type.Int, Location(3, 5)),
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
                                                                AST.Expression.Constant(AST.IntConstant(2), Type.Int, Location(4, 13)),
                                                                type = Type.Int,
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
                            type = Type.Function(parameters = emptyList(), returnType = Type.Int),
                            storageClass = null,
                            location = Location(1, 1),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                symbolTable = mapOf(
                    "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                    "a.0" to Type.Int.toSymbol(),
                    "a.1" to Type.Int.toSymbol(),
                ),
            )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should allow using variable from outer scope`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                if_(1.e) {
                    assign("a".e, 2.e)
                }
            }
        }

        val expected = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    if_(1.e.int()) {
                        assign("a.0".e.int(), 2.e.int(), type = Type.Int)
                    }
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should allow first using and then shadowing variable from outer scope`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                int("a") assign 1.e
                nested {
                    assign("a".e, 1.e)
                    int("a") assign 2.e
                }
            }
        }

        val expected = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    int("a.0") assign 1.e.int()
                    nested {
                        assign("a.0".e.int(), 1.e.int(), type = Type.Int)
                        int("a.1") assign 2.e.int()
                    }
                }
            },
            renamedVariableCount = 2,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Int.toSymbol(),
                "a.1" to Type.Int.toSymbol(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }

    @Test
    fun `should not allow declaring variable multiple times in nested scope`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
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

    @Test
    fun `should insert a cast if right side of assignment needs it`() {
        val input = program {
            function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                long("a") assign 1.e
            }
        }

        val expected = ValidASTProgram(
            value = program {
                function("main", Type.Function(parameters = emptyList(), returnType = Type.Int)) {
                    long("a.0") assign cast(Type.Long, 1.e.int()).long()
                }
            },
            renamedVariableCount = 1,
            symbolTable = mapOf(
                "main" to Type.Function(parameters = emptyList(), returnType = Type.Int).toSymbol(),
                "a.0" to Type.Long.toSymbol(),
            ),
        )

        assertEquals(expected.right(), semanticAnalysis(input))
    }
}
