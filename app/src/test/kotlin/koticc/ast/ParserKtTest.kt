package koticc.ast

import arrow.core.left
import arrow.core.right
import koticc.VarargArgumentsProvider
import koticc.parseInput
import koticc.token.Location
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.Test

class ParserKtTest {
    @Test
    fun `should return AST for empty function`() {
        val input =
            """
int main(void) {
}
      """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body = AST.Block(blockItems = emptyList()),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    class FunctionDefinitionErrorTestCases :
        VarargArgumentsProvider(
            "main" to
                ParserError(
                    "expected token IntKeyword, but got Identifier(value=main)",
                    Location(1, 1),
                ),
            "int {" to ParserError("expected identifier, got OpenBrace", Location(1, 5)),
            "int main {" to
                ParserError("expected token OpenParen, but got OpenBrace", Location(1, 10)),
            "int test() {" to ParserError("expected void or int, got CloseParen", Location(1, 10)),
            "int main(void) {}abc" to
                ParserError("expected token IntKeyword, but got Identifier(value=abc)", Location(1, 18)),
        )

    @ParameterizedTest
    @ArgumentsSource(FunctionDefinitionErrorTestCases::class)
    fun `should return error for unexpected token while parsing function definition`(
        input: String,
        expected: ParserError,
    ) {
        assertEquals(expected.left(), parseInput(input))
    }

    @Test
    fun `should parse declaration without initializer`() {
        val input =
            """
           int main(void) {
               int a;
           }
       """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    AST.Declaration.Variable("a", null, Location(2, 5)),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should parse declaration with initializer`() {
        val input =
            """
           int main(void) {
               int a = 1;
           }
       """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Declaration(
                                    AST.Declaration.Variable(
                                        "a",
                                        AST.Expression.IntLiteral(
                                            value = 1,
                                            location = Location(2, 13),
                                        ),
                                        Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should parse return statement`() {
        val input =
            """
           int main(void) {
               return 1;
           }
       """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Statement(
                                    AST.Statement.Return(
                                        AST.Expression.IntLiteral(
                                            value = 1,
                                            location = Location(2, 12),
                                        ),
                                        Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should parse null statement`() {
        val input =
            """
           int main(void) {
               ;
           }
       """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Statement(
                                    AST.Statement.Null(
                                        location = Location(2, 5),
                                    ),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    class ExpressionTestCases :
        VarargArgumentsProvider(
            "1" to AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
            "a" to AST.Expression.Variable(name = "a", location = Location(2, 5)),
            "(1)" to AST.Expression.IntLiteral(value = 1, location = Location(2, 6)),
            "-1" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.Negate,
                    operand = AST.Expression.IntLiteral(value = 1, location = Location(2, 6)),
                    location = Location(2, 5),
                ),
            "~1" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.Complement,
                    operand = AST.Expression.IntLiteral(value = 1, location = Location(2, 6)),
                    location = Location(2, 5),
                ),
            "!1" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.LogicalNegate,
                    operand = AST.Expression.IntLiteral(value = 1, location = Location(2, 6)),
                    location = Location(2, 5),
                ),
            "-(!(~2))" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.Negate,
                    operand =
                    AST.Expression.Unary(
                        operator = AST.UnaryOperator.LogicalNegate,
                        operand =
                        AST.Expression.Unary(
                            operator = AST.UnaryOperator.Complement,
                            operand =
                            AST.Expression.IntLiteral(
                                value = 2,
                                location = Location(2, 10),
                            ),
                            location = Location(2, 9),
                        ),
                        location = Location(2, 7),
                    ),
                    location = Location(2, 5),
                ),
            *listOf(
                "+" to AST.BinaryOperator.Add,
                "-" to AST.BinaryOperator.Subtract,
                "*" to AST.BinaryOperator.Multiply,
                "/" to AST.BinaryOperator.Divide,
                "%" to AST.BinaryOperator.Modulo,
                "==" to AST.BinaryOperator.Equal,
                "!=" to AST.BinaryOperator.NotEqual,
                "<" to AST.BinaryOperator.LessThan,
                "<=" to AST.BinaryOperator.LessThanOrEqual,
                ">" to AST.BinaryOperator.GreaterThan,
                ">=" to AST.BinaryOperator.GreaterThanOrEqual,
                "&&" to AST.BinaryOperator.LogicalAnd,
                "||" to AST.BinaryOperator.LogicalOr,
                "&" to AST.BinaryOperator.BitwiseAnd,
                "|" to AST.BinaryOperator.BitwiseOr,
                "^" to AST.BinaryOperator.BitwiseXor,
                "<<" to AST.BinaryOperator.ShiftLeft,
                ">>" to AST.BinaryOperator.ShiftRight,
            )
                .flatMap { (token, operator) ->
                    listOf(
                        "1 $token 2" to
                            AST.Expression.Binary(
                                operator = operator,
                                left = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                                right =
                                AST.Expression.IntLiteral(
                                    value = 2,
                                    location = Location(2, 8 + token.length),
                                ),
                            ),
                        "1 $token 2 $token 3" to
                            AST.Expression.Binary(
                                operator = operator,
                                left =
                                AST.Expression.Binary(
                                    operator = operator,
                                    left =
                                    AST.Expression.IntLiteral(
                                        value = 1,
                                        location = Location(2, 5),
                                    ),
                                    right =
                                    AST.Expression.IntLiteral(
                                        value = 2,
                                        location = Location(2, 8 + token.length),
                                    ),
                                ),
                                right =
                                AST.Expression.IntLiteral(
                                    value = 3,
                                    location = Location(2, 11 + 2 * token.length),
                                ),
                            ),
                    )
                }
                .toTypedArray(),
            "1 + 2 - 3" to
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Subtract,
                    left =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.Add,
                        left = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 9)),
                    ),
                    right = AST.Expression.IntLiteral(value = 3, location = Location(2, 13)),
                ),
            "1 + 2 * 3" to
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Add,
                    left = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                    right =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.Multiply,
                        left = AST.Expression.IntLiteral(value = 2, location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 3, location = Location(2, 13)),
                    ),
                ),
            "1 * 2 + 3" to
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Add,
                    left =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.Multiply,
                        left = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 9)),
                    ),
                    right = AST.Expression.IntLiteral(value = 3, location = Location(2, 13)),
                ),
            "a = 1 + 2" to
                AST.Expression.Assignment(
                    left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    right =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.Add,
                        left = AST.Expression.IntLiteral(value = 1, location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 13)),
                    ),
                ),
            "a = b = 3" to
                AST.Expression.Assignment(
                    left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    right =
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "b", location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 3, location = Location(2, 13)),
                    ),
                ),
            *listOf(
                "+=" to AST.CompoundAssignmentOperator.Add,
                "-=" to AST.CompoundAssignmentOperator.Subtract,
                "*=" to AST.CompoundAssignmentOperator.Multiply,
                "/=" to AST.CompoundAssignmentOperator.Divide,
                "%=" to AST.CompoundAssignmentOperator.Modulo,
                "&=" to AST.CompoundAssignmentOperator.BitwiseAnd,
                "|=" to AST.CompoundAssignmentOperator.BitwiseOr,
                "^=" to AST.CompoundAssignmentOperator.BitwiseXor,
                "<<=" to AST.CompoundAssignmentOperator.ShiftLeft,
                ">>=" to AST.CompoundAssignmentOperator.ShiftRight,
            )
                .map { (token, operator) ->
                    "a $token 1 + 2" to
                        AST.Expression.CompoundAssignment(
                            operator = operator,
                            left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                            right =
                            AST.Expression.Binary(
                                operator = AST.BinaryOperator.Add,
                                left =
                                AST.Expression.IntLiteral(
                                    value = 1,
                                    location = Location(2, 8 + token.length),
                                ),
                                right =
                                AST.Expression.IntLiteral(
                                    value = 2,
                                    location = Location(2, 12 + token.length),
                                ),
                            ),
                        )
                }
                .toTypedArray(),
            "a = b += c = b &= 4" to
                AST.Expression.Assignment(
                    left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    right =
                    AST.Expression.CompoundAssignment(
                        operator = AST.CompoundAssignmentOperator.Add,
                        left =
                        AST.Expression.Variable(
                            name = "b",
                            location = Location(2, 9),
                        ),
                        right =
                        AST.Expression.Assignment(
                            left =
                            AST.Expression.Variable(
                                name = "c",
                                location = Location(2, 14),
                            ),
                            right =
                            AST.Expression.CompoundAssignment(
                                operator = AST.CompoundAssignmentOperator.BitwiseAnd,
                                left =
                                AST.Expression.Variable(
                                    name = "b",
                                    location = Location(2, 18),
                                ),
                                right =
                                AST.Expression.IntLiteral(
                                    value = 4,
                                    location = Location(2, 23),
                                ),
                            ),
                        ),
                    ),
                ),
            "++i" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Add,
                    left = AST.Expression.Variable(name = "i", location = Location(2, 7)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "++++i" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Add,
                    left =
                    AST.Expression.CompoundAssignment(
                        operator = AST.CompoundAssignmentOperator.Add,
                        left = AST.Expression.Variable(name = "i", location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 7)),
                    ),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "-++i" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.Negate,
                    operand =
                    AST.Expression.CompoundAssignment(
                        operator = AST.CompoundAssignmentOperator.Add,
                        left = AST.Expression.Variable(name = "i", location = Location(2, 8)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 6)),
                    ),
                    location = Location(2, 5),
                ),
            "--i" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Subtract,
                    left = AST.Expression.Variable(name = "i", location = Location(2, 7)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "----i" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Subtract,
                    left =
                    AST.Expression.CompoundAssignment(
                        operator = AST.CompoundAssignmentOperator.Subtract,
                        left = AST.Expression.Variable(name = "i", location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 7)),
                    ),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "++3" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Add,
                    left = AST.Expression.IntLiteral(value = 3, location = Location(2, 7)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "i++" to
                AST.Expression.Postfix(
                    operator = AST.PostfixOperator.Increment,
                    operand = AST.Expression.Variable(name = "i", location = Location(2, 5)),
                ),
            "i++++" to
                AST.Expression.Postfix(
                    operator = AST.PostfixOperator.Increment,
                    operand =
                    AST.Expression.Postfix(
                        operator = AST.PostfixOperator.Increment,
                        operand = AST.Expression.Variable(name = "i", location = Location(2, 5)),
                    ),
                ),
            "-i++" to
                AST.Expression.Unary(
                    operator = AST.UnaryOperator.Negate,
                    operand =
                    AST.Expression.Postfix(
                        operator = AST.PostfixOperator.Increment,
                        operand = AST.Expression.Variable(name = "i", location = Location(2, 6)),
                    ),
                    location = Location(2, 5),
                ),
            "i--" to
                AST.Expression.Postfix(
                    operator = AST.PostfixOperator.Decrement,
                    operand = AST.Expression.Variable(name = "i", location = Location(2, 5)),
                ),
            "(i)++" to
                AST.Expression.Postfix(
                    operator = AST.PostfixOperator.Increment,
                    operand = AST.Expression.Variable(name = "i", location = Location(2, 6)),
                ),
            "++i++" to
                AST.Expression.CompoundAssignment(
                    operator = AST.CompoundAssignmentOperator.Add,
                    left =
                    AST.Expression.Postfix(
                        operator = AST.PostfixOperator.Increment,
                        operand = AST.Expression.Variable(name = "i", location = Location(2, 7)),
                    ),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                ),
            "i = j++" to
                AST.Expression.Assignment(
                    left = AST.Expression.Variable(name = "i", location = Location(2, 5)),
                    right =
                    AST.Expression.Postfix(
                        operator = AST.PostfixOperator.Increment,
                        operand = AST.Expression.Variable(name = "j", location = Location(2, 9)),
                    ),
                ),
            "i++ + ++j" to
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Add,
                    left =
                    AST.Expression.Postfix(
                        operator = AST.PostfixOperator.Increment,
                        operand = AST.Expression.Variable(name = "i", location = Location(2, 5)),
                    ),
                    right =
                    AST.Expression.CompoundAssignment(
                        operator = AST.CompoundAssignmentOperator.Add,
                        left = AST.Expression.Variable(name = "j", location = Location(2, 13)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 11)),
                    ),
                ),
            "a ? 1 : 2" to
                AST.Expression.Conditional(
                    condition = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    thenExpression = AST.Expression.IntLiteral(value = 1, location = Location(2, 9)),
                    elseExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 13)),
                ),
            "a ? b ? 1 : 2 : 3" to
                AST.Expression.Conditional(
                    condition = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    thenExpression =
                    AST.Expression.Conditional(
                        condition = AST.Expression.Variable(name = "b", location = Location(2, 9)),
                        thenExpression = AST.Expression.IntLiteral(value = 1, location = Location(2, 13)),
                        elseExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 17)),
                    ),
                    elseExpression = AST.Expression.IntLiteral(value = 3, location = Location(2, 21)),
                ),
            "a ? 1 : b ? 2 : 3" to
                AST.Expression.Conditional(
                    condition = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    thenExpression = AST.Expression.IntLiteral(value = 1, location = Location(2, 9)),
                    elseExpression =
                    AST.Expression.Conditional(
                        condition = AST.Expression.Variable(name = "b", location = Location(2, 13)),
                        thenExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 17)),
                        elseExpression = AST.Expression.IntLiteral(value = 3, location = Location(2, 21)),
                    ),
                ),
            "a = 1 ? 2 : 3" to
                AST.Expression.Assignment(
                    left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                    right =
                    AST.Expression.Conditional(
                        condition = AST.Expression.IntLiteral(value = 1, location = Location(2, 9)),
                        thenExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 13)),
                        elseExpression = AST.Expression.IntLiteral(value = 3, location = Location(2, 17)),
                    ),
                ),
            "a || b ? 1 : 2" to
                AST.Expression.Conditional(
                    condition =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.LogicalOr,
                        left = AST.Expression.Variable(name = "a", location = Location(2, 5)),
                        right = AST.Expression.Variable(name = "b", location = Location(2, 10)),
                    ),
                    thenExpression = AST.Expression.IntLiteral(value = 1, location = Location(2, 14)),
                    elseExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 18)),
                ),
            "1 ? 2 : 3 || 4" to
                AST.Expression.Conditional(
                    condition = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                    thenExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 9)),
                    elseExpression =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.LogicalOr,
                        left = AST.Expression.IntLiteral(value = 3, location = Location(2, 13)),
                        right = AST.Expression.IntLiteral(value = 4, location = Location(2, 18)),
                    ),
                ),
            "1 ? a = 1 : 2" to
                AST.Expression.Conditional(
                    condition = AST.Expression.IntLiteral(value = 1, location = Location(2, 5)),
                    thenExpression =
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "a", location = Location(2, 9)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 13)),
                    ),
                    elseExpression = AST.Expression.IntLiteral(value = 2, location = Location(2, 17)),
                ),
        )

    @ParameterizedTest
    @ArgumentsSource(ExpressionTestCases::class)
    fun `should parse expression statement`(
        expressionInput: String,
        expectedExpression: AST.Expression,
    ) {
        val input =
            """
           int main(void) {
               $expressionInput;
           }
       """
                .trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Statement(
                                    AST.Statement.Expression(expectedExpression),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            )
                .right()

        assertEquals(expected, parseInput(input))
    }

    class IfStatementTestCases : VarargArgumentsProvider(
        "if (a == 1) b = 2;" to
            AST.Statement.If(
                condition =
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Equal,
                    left = AST.Expression.Variable(name = "a", location = Location(2, 9)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 14)),
                ),
                thenStatement =
                AST.Statement.Expression(
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "b", location = Location(2, 17)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 21)),
                    ),
                ),
                elseStatement = null,
            ),
        "if (a) b = 1; else b = 2;" to
            AST.Statement.If(
                condition = AST.Expression.Variable(name = "a", location = Location(2, 9)),
                thenStatement =
                AST.Statement.Expression(
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "b", location = Location(2, 12)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(2, 16)),
                    ),
                ),
                elseStatement =
                AST.Statement.Expression(
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "b", location = Location(2, 24)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 28)),
                    ),
                ),
            ),
        "if (a == 1) b = 1; else if (a == 2) b = 2; else if (a == 3) b = 3;" to
            AST.Statement.If(
                condition =
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Equal,
                    left = AST.Expression.Variable(name = "a", location = Location(line = 2, column = 9)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(line = 2, column = 14)),
                ),
                thenStatement =
                AST.Statement.Expression(
                    expression =
                    AST.Expression.Assignment(
                        left = AST.Expression.Variable(name = "b", location = Location(line = 2, column = 17)),
                        right = AST.Expression.IntLiteral(value = 1, location = Location(line = 2, column = 21)),
                    ),
                ),
                elseStatement =
                AST.Statement.If(
                    condition =
                    AST.Expression.Binary(
                        operator = AST.BinaryOperator.Equal,
                        left = AST.Expression.Variable(name = "a", location = Location(line = 2, column = 33)),
                        right = AST.Expression.IntLiteral(value = 2, location = Location(line = 2, column = 38)),
                    ),
                    thenStatement =
                    AST.Statement.Expression(
                        expression =
                        AST.Expression.Assignment(
                            left = AST.Expression.Variable(name = "b", location = Location(line = 2, column = 41)),
                            right = AST.Expression.IntLiteral(value = 2, location = Location(line = 2, column = 45)),
                        ),
                    ),
                    elseStatement =
                    AST.Statement.If(
                        condition =
                        AST.Expression.Binary(
                            operator = AST.BinaryOperator.Equal,
                            left = AST.Expression.Variable(name = "a", location = Location(line = 2, column = 57)),
                            right = AST.Expression.IntLiteral(value = 3, location = Location(line = 2, column = 62)),
                        ),
                        thenStatement =
                        AST.Statement.Expression(
                            expression =
                            AST.Expression.Assignment(
                                left = AST.Expression.Variable(name = "b", location = Location(line = 2, column = 65)),
                                right = AST.Expression.IntLiteral(value = 3, location = Location(line = 2, column = 69)),
                            ),
                        ),
                        elseStatement = null,
                    ),
                ),
            ),
        "if (a == 1) { int c = 1; b = c + 1; } else { b = 2; }" to
            AST.Statement.If(
                condition =
                AST.Expression.Binary(
                    operator = AST.BinaryOperator.Equal,
                    left = AST.Expression.Variable(name = "a", location = Location(2, 9)),
                    right = AST.Expression.IntLiteral(value = 1, location = Location(2, 14)),
                ),
                thenStatement =
                AST.Statement.Compound(
                    block =
                    AST.Block(
                        blockItems =
                        listOf(
                            AST.BlockItem.Declaration(
                                declaration =
                                AST.Declaration.Variable(
                                    name = "c",
                                    initializer = AST.Expression.IntLiteral(value = 1, location = Location(2, 27)),
                                    location = Location(2, 19),
                                ),
                            ),
                            AST.BlockItem.Statement(
                                AST.Statement.Expression(
                                    AST.Expression.Assignment(
                                        left = AST.Expression.Variable(name = "b", location = Location(2, 30)),
                                        right =
                                        AST.Expression.Binary(
                                            operator = AST.BinaryOperator.Add,
                                            left = AST.Expression.Variable(name = "c", location = Location(2, 34)),
                                            right = AST.Expression.IntLiteral(value = 1, location = Location(2, 38)),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                elseStatement =
                AST.Statement.Compound(
                    block =
                    AST.Block(
                        blockItems =
                        listOf(
                            AST.BlockItem.Statement(
                                AST.Statement.Expression(
                                    AST.Expression.Assignment(
                                        left = AST.Expression.Variable(name = "b", location = Location(2, 50)),
                                        right = AST.Expression.IntLiteral(value = 2, location = Location(2, 54)),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
    )

    @ParameterizedTest
    @ArgumentsSource(IfStatementTestCases::class)
    fun `should parse if statement`(
        ifStatementInput: String,
        expectedStatement: AST.Statement.If,
    ) {
        val input =
            """
            int main(void) {
                $ifStatementInput
            }
            """.trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body = AST.Block(blockItems = listOf(AST.BlockItem.Statement(expectedStatement))),
                        location = Location(1, 1),
                    ),
                ),
            ).right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should parse top level compound statement in function body`() {
        val input =
            """
            int main(void) {
                {
                    int a = 1;
                    int b = 2;
                }
            }
            """.trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Statement(
                                    AST.Statement.Compound(
                                        block =
                                        AST.Block(
                                            blockItems =
                                            listOf(
                                                AST.BlockItem.Declaration(
                                                    AST.Declaration.Variable(
                                                        name = "a",
                                                        initializer =
                                                        AST.Expression.IntLiteral(
                                                            value = 1,
                                                            location = Location(3, 17),
                                                        ),
                                                        location = Location(3, 9),
                                                    ),
                                                ),
                                                AST.BlockItem.Declaration(
                                                    AST.Declaration.Variable(
                                                        name = "b",
                                                        initializer =
                                                        AST.Expression.IntLiteral(
                                                            value = 2,
                                                            location = Location(4, 17),
                                                        ),
                                                        location = Location(4, 9),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            ).right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should parse labeled statement and goto`() {
        val input =
            """
            int main(void) {
            label:
                a = 1;
                goto label;
            }
            """.trimIndent()

        val expected =
            AST.Program(
                functionDeclarations =
                listOf(
                    AST.Declaration.Function(
                        name = "main",
                        parameters = emptyList(),
                        body =
                        AST.Block(
                            blockItems =
                            listOf(
                                AST.BlockItem.Statement(
                                    AST.Statement.Labeled(
                                        label = LabelName("label"),
                                        statement =
                                        AST.Statement.Expression(
                                            AST.Expression.Assignment(
                                                left = AST.Expression.Variable(name = "a", location = Location(3, 5)),
                                                right = AST.Expression.IntLiteral(value = 1, location = Location(3, 9)),
                                            ),
                                        ),
                                        location = Location(2, 1),
                                    ),
                                ),
                                AST.BlockItem.Statement(
                                    AST.Statement.Goto(
                                        label = LabelName("label"),
                                        location = Location(4, 5),
                                    ),
                                ),
                            ),
                        ),
                        location = Location(1, 1),
                    ),
                ),
            ).right()

        assertEquals(expected, parseInput(input))
    }

    @Test
    fun `should return error for unexpected token while parsing expression`() {
        val input =
            """
           int main(void) {
               1 +;
           }
       """
                .trimIndent()

        val expected = ParserError("expected factor, got Semicolon", Location(2, 8)).left()

        assertEquals(expected, parseInput(input))
    }
}
