@file:Suppress("ktlint:standard:filename")

package koticc

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.right

data class ParserError(val message: String, val location: Location?) : CompilerError {
    override fun message(): String = "parser error, at ${location?.toHumanReadableString() ?: "EOF"}: $message"
}

fun parse(tokens: List<TokenWithLocation>): Either<ParserError, AST.Program> = Parser(tokens).parse()

private class Parser(
    val tokens: List<TokenWithLocation>,
) {
    var current = 0

    fun parse(): Either<ParserError, AST.Program> =
        either {
            val functionDefinition = parseFunctionDefinition().bind()

            val nextToken = nextToken()
            ensure(nextToken == null) {
                ParserError("expected EOF, but got ${nextToken?.value}", nextToken?.location)
            }
            AST.Program(functionDefinition)
        }

    private fun peekToken(): TokenWithLocation? = tokens.getOrNull(current)

    private fun peekTokenAfterPeek(): TokenWithLocation? = tokens.getOrNull(current + 1)

    private fun nextToken(): TokenWithLocation? {
        if (current >= tokens.size) {
            return null
        }
        return tokens[current++]
    }

    private fun expectToken(expected: Token): Either<ParserError, TokenWithLocation> {
        val nextToken = nextToken()
        return if (nextToken?.value == expected) {
            nextToken.right()
        } else {
            if (nextToken == null) {
                ParserError("expected token $expected, but got EOF", null).left()
            } else {
                ParserError("expected token $expected, but got ${nextToken.value}", nextToken.location)
                    .left()
            }
        }
    }

    // TODO: do we really need to return location as well
    private fun expectIdentifier(): Either<ParserError, TokenIdentifierWithLocation> {
        val nextToken = nextToken()
        return when (val tokenValue = nextToken?.value) {
            is Token.Identifier -> TokenIdentifierWithLocation(tokenValue, nextToken.location).right()
            else ->
                ParserError("expected identifier, got ${nextToken?.value ?: "EOF"}", nextToken?.location)
                    .left()
        }
    }

    private fun parseFunctionDefinition(): Either<ParserError, AST.FunctionDefinition> =
        either {
            val returnTypeToken = expectToken(Token.IntKeyword).bind()
            val nameToken = expectIdentifier().bind()
            expectToken(Token.OpenParen).bind()
            expectToken(Token.Void).bind()
            expectToken(Token.CloseParen).bind()
            expectToken(Token.OpenBrace).bind()

            val body = mutableListOf<AST.BlockItem>()
            while (peekToken()?.value != Token.CloseBrace) {
                body.add(parseBlockItem().bind())
            }
            expectToken(Token.CloseBrace).bind()

            return AST.FunctionDefinition(
                name = nameToken.value.value,
                body = body,
                location = returnTypeToken.location,
            ).right()
        }

    private fun parseBlockItem(): Either<ParserError, AST.BlockItem> =
        either {
            when (peekToken()?.value) {
                is Token.IntKeyword -> {
                    AST.BlockItem.Declaration(parseDeclaration().bind())
                }
                else -> {
                    AST.BlockItem.Statement(parseStatement().bind())
                }
            }
        }

    private fun parseDeclaration(): Either<ParserError, AST.Declaration> =
        either {
            val typeToken = expectToken(Token.IntKeyword).bind()
            val nameToken = expectIdentifier().bind()
            val initializer =
                if (peekToken()?.value == Token.Equal) {
                    nextToken()
                    parseExpression(0).bind()
                } else {
                    null
                }
            expectToken(Token.Semicolon).bind()
            AST.Declaration(nameToken.value.value, initializer, typeToken.location)
        }

    private fun parseStatement(): Either<ParserError, AST.Statement> =
        either {
            val peekToken = peekToken()
            when (peekToken?.value) {
                Token.Return -> {
                    nextToken()
                    val expression = parseExpression(0).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Return(expression, peekToken.location)
                }
                Token.Semicolon -> {
                    nextToken()
                    AST.Statement.Null(peekToken.location)
                }
                Token.If -> {
                    nextToken()
                    expectToken(Token.OpenParen).bind()
                    val condition = parseExpression(0).bind()
                    expectToken(Token.CloseParen).bind()
                    val thenStatement = parseStatement().bind()
                    val elseStatement =
                        if (peekToken()?.value == Token.Else) {
                            nextToken()
                            parseStatement().bind()
                        } else {
                            null
                        }
                    AST.Statement.If(condition, thenStatement, elseStatement)
                }
                is Token.Identifier -> {
                    if (peekTokenAfterPeek()?.value == Token.Colon) {
                        val labelToken = expectIdentifier().bind()
                        expectToken(Token.Colon).bind()
                        val statement = parseStatement().bind()
                        AST.Statement.Labeled(LabelName(labelToken.value.value), statement, labelToken.location)
                    } else {
                        val expression = parseExpression(0).bind()
                        expectToken(Token.Semicolon).bind()
                        AST.Statement.Expression(expression)
                    }
                }
                Token.Goto -> {
                    val gotoToken = expectToken(Token.Goto).bind()
                    val labelToken = expectIdentifier().bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Goto(LabelName(labelToken.value.value), gotoToken.location)
                }
                else -> {
                    val expression = parseExpression(0).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Expression(expression)
                }
            }
        }

    private fun parseExpression(minPrecedence: Int): Either<ParserError, AST.Expression> =
        either {
            var left = parseFactor().bind()
            var binaryOperatorLike = peekToken()?.let(::toBinaryOperatorLikeOrNull)
            while (binaryOperatorLike != null) {
                val precedence = binaryOperatorLike.precedence()
                if (precedence < minPrecedence) {
                    break
                }
                nextToken()
                left =
                    when (binaryOperatorLike) {
                        is BinaryOperatorLike.BinaryOperator ->
                            AST.Expression.Binary(
                                operator = binaryOperatorLike.value,
                                left = left,
                                // minPrecedence = precedence + 1 => left associative
                                right = parseExpression(precedence + 1).bind(),
                            )
                        is BinaryOperatorLike.Assignment ->
                            AST.Expression.Assignment(
                                left = left,
                                // minPrecedence = precedence => right associative
                                right = parseExpression(precedence).bind(),
                            )
                        is BinaryOperatorLike.CompoundAssignmentOperator ->
                            AST.Expression.CompoundAssignment(
                                operator = binaryOperatorLike.value,
                                left = left,
                                // minPrecedence = precedence => right associative
                                right = parseExpression(precedence).bind(),
                            )
                        is BinaryOperatorLike.Conditional -> {
                            // any expression can be between ? and : - even assignment
                            val thenExpression = parseExpression(0).bind()
                            expectToken(Token.Colon).bind()
                            // minPrecedence = precedence => right associative
                            val elseExpression = parseExpression(precedence).bind()
                            AST.Expression.Conditional(
                                condition = left,
                                thenExpression = thenExpression,
                                elseExpression = elseExpression,
                            )
                        }
                    }
                binaryOperatorLike = peekToken()?.let(::toBinaryOperatorLikeOrNull)
            }

            left
        }

    private fun parseFactor(): Either<ParserError, AST.Expression> =
        either {
            val peekToken = peekToken()
            var factor =
                when (val peekTokenValue = peekToken?.value) {
                    is Token.IntLiteral -> {
                        nextToken()
                        AST.Expression.IntLiteral(peekTokenValue.value, peekToken.location)
                    }
                    is Token.Identifier -> {
                        nextToken()
                        AST.Expression.Variable(peekTokenValue.value, peekToken.location)
                    }
                    is Token.OpenParen -> {
                        nextToken()
                        val expression = parseExpression(0).bind()
                        expectToken(Token.CloseParen).bind()
                        expression
                    }
                    is Token.Minus,
                    Token.Tilde,
                    Token.Exclamation,
                    -> {
                        parseUnary().bind()
                    }
                    is Token.DoublePlus, Token.DoubleMinus -> {
                        parseFactorWithPossiblePrefix().bind()
                    }
                    else -> raise(ParserError("expected factor, got $peekTokenValue", peekToken?.location))
                }
            var postfixOperator = peekToken()?.value?.toPostfixOperatorOrNull()
            while (postfixOperator != null) {
                nextToken()
                factor =
                    AST.Expression.Postfix(
                        operator = postfixOperator,
                        operand = factor,
                    )
                postfixOperator = peekToken()?.value?.toPostfixOperatorOrNull()
            }

            factor
        }

    private fun parseFactorWithPossiblePrefix(): Either<ParserError, AST.Expression> =
        either {
            val peekToken = peekToken()
            val operator =
                when (peekToken?.value) {
                    Token.DoublePlus -> AST.CompoundAssignmentOperator.Add
                    Token.DoubleMinus -> AST.CompoundAssignmentOperator.Subtract
                    else ->
                        raise(
                            ParserError(
                                "expected prefix increment or decrement, got ${peekToken?.value}",
                                peekToken?.location,
                            ),
                        )
                }
            nextToken()
            val operand = parseFactor().bind()
            AST.Expression.CompoundAssignment(
                operator = operator,
                left = operand,
                right = AST.Expression.IntLiteral(1, peekToken.location),
            )
        }

    private fun parseUnary(): Either<ParserError, AST.Expression.Unary> =
        either {
            val peekToken = peekToken()
            val unaryOperator =
                when (peekToken?.value) {
                    Token.Minus -> AST.UnaryOperator.Negate
                    Token.Tilde -> AST.UnaryOperator.Complement
                    Token.Exclamation -> AST.UnaryOperator.LogicalNegate
                    else ->
                        raise(
                            ParserError(
                                "expected unary operator, got ${peekToken?.value}",
                                peekToken?.location,
                            ),
                        )
                }
            nextToken()
            val operand = parseFactor().bind()
            AST.Expression.Unary(unaryOperator, operand, peekToken.location)
        }

    private fun toBinaryOperatorLikeOrNull(token: TokenWithLocation): BinaryOperatorLike? {
        return when (token.value) {
            Token.Plus -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Add, token.location)
            Token.Minus -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Subtract, token.location)
            Token.Asterisk ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Multiply, token.location)
            Token.Slash -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Divide, token.location)
            Token.Percent -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Modulo, token.location)
            Token.DoubleEqual ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.Equal, token.location)
            Token.ExclamationEqual ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.NotEqual, token.location)
            Token.LessThan ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.LessThan, token.location)
            Token.LessThanOrEqual ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.LessThanOrEqual, token.location)
            Token.GreaterThan ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.GreaterThan, token.location)
            Token.GreaterThanOrEqual ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.GreaterThanOrEqual, token.location)
            Token.DoubleAmpersand ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.LogicalAnd, token.location)
            Token.DoublePipe ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.LogicalOr, token.location)
            Token.Ampersand ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.BitwiseAnd, token.location)
            Token.Pipe -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.BitwiseOr, token.location)
            Token.Caret -> BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.BitwiseXor, token.location)
            Token.DoubleLessThan ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.ShiftLeft, token.location)
            Token.DoubleGreaterThan ->
                BinaryOperatorLike.BinaryOperator(AST.BinaryOperator.ShiftRight, token.location)
            Token.Equal -> BinaryOperatorLike.Assignment(token.location)
            Token.PlusEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.Add,
                    token.location,
                )
            Token.MinusEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.Subtract,
                    token.location,
                )
            Token.AsteriskEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.Multiply,
                    token.location,
                )
            Token.SlashEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.Divide,
                    token.location,
                )
            Token.PercentEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.Modulo,
                    token.location,
                )
            Token.AmpersandEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.BitwiseAnd,
                    token.location,
                )
            Token.CaretEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.BitwiseXor,
                    token.location,
                )
            Token.PipeEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.BitwiseOr,
                    token.location,
                )
            Token.DoubleLessThanEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.ShiftLeft,
                    token.location,
                )
            Token.DoubleGreaterThanEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.CompoundAssignmentOperator.ShiftRight,
                    token.location,
                )
            Token.QuestionMark -> BinaryOperatorLike.Conditional
            else -> null
        }
    }

    private fun BinaryOperatorLike.precedence() =
        when (this) {
            is BinaryOperatorLike.Assignment -> 1
            is BinaryOperatorLike.CompoundAssignmentOperator -> 1
            is BinaryOperatorLike.Conditional -> 2
            is BinaryOperatorLike.BinaryOperator ->
                when (value) {
                    AST.BinaryOperator.LogicalOr -> 34
                    AST.BinaryOperator.LogicalAnd -> 35
                    AST.BinaryOperator.BitwiseOr -> 37
                    AST.BinaryOperator.BitwiseXor -> 38
                    AST.BinaryOperator.BitwiseAnd -> 39
                    AST.BinaryOperator.Equal,
                    AST.BinaryOperator.NotEqual,
                    -> 40
                    AST.BinaryOperator.LessThan,
                    AST.BinaryOperator.LessThanOrEqual,
                    AST.BinaryOperator.GreaterThan,
                    AST.BinaryOperator.GreaterThanOrEqual,
                    -> 41
                    AST.BinaryOperator.ShiftLeft,
                    AST.BinaryOperator.ShiftRight,
                    -> 42
                    AST.BinaryOperator.Add,
                    AST.BinaryOperator.Subtract,
                    -> 50
                    AST.BinaryOperator.Multiply,
                    AST.BinaryOperator.Divide,
                    AST.BinaryOperator.Modulo,
                    -> 60
                }
        }

    private fun Token.toPostfixOperatorOrNull() =
        when (this) {
            Token.DoublePlus -> AST.PostfixOperator.Increment
            Token.DoubleMinus -> AST.PostfixOperator.Decrement
            else -> null
        }
}

private data class TokenIdentifierWithLocation(
    val value: Token.Identifier,
    val location: Location,
)

private sealed interface BinaryOperatorLike {
    data class BinaryOperator(val value: AST.BinaryOperator, val location: Location) :
        BinaryOperatorLike

    data class Assignment(val location: Location) : BinaryOperatorLike

    data class CompoundAssignmentOperator(
        val value: AST.CompoundAssignmentOperator,
        val location: Location,
    ) : BinaryOperatorLike

    data object Conditional : BinaryOperatorLike
}

private sealed interface Associativity {
    data object Left : Associativity

    data object Right : Associativity
}
