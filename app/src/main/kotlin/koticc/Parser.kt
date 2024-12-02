@file:Suppress("ktlint:standard:filename")

package koticc

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
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
            val functionDeclarations = mutableListOf<AST.Declaration.Function>()
            while (peekToken() != null) {
                functionDeclarations.add(parseFunctionDeclaration().bind())
            }
            AST.Program(functionDeclarations)
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

    private fun expectIdentifier(): Either<ParserError, TokenIdentifierWithLocation> {
        val nextToken = nextToken()
        return when (val tokenValue = nextToken?.value) {
            is Token.Identifier -> TokenIdentifierWithLocation(tokenValue, nextToken.location).right()
            else ->
                ParserError("expected identifier, got ${nextToken?.value ?: "EOF"}", nextToken?.location)
                    .left()
        }
    }

    private fun expectFunctionParameter(): Either<ParserError, AST.FunctionParameter> =
        either {
            val nameToken = expectIdentifier().bind()
            AST.FunctionParameter(nameToken.value.value, nameToken.location)
        }

    private fun parseFunctionDeclaration(): Either<ParserError, AST.Declaration.Function> =
        either {
            val returnTypeToken = expectToken(Token.IntKeyword).bind()
            val nameToken = expectIdentifier().bind()
            expectToken(Token.OpenParen).bind()
            val parameters = parseFunctionDeclarationParameters().bind()
            expectToken(Token.CloseParen).bind()

            val peekToken = peekToken()
            val body = when (peekToken?.value) {
                Token.OpenBrace -> parseBlock().bind()
                Token.Semicolon -> {
                    nextToken()
                    null
                }
                else -> raise(ParserError("expected block or semicolon, got ${peekToken?.value}", peekToken?.location))
            }

            return AST.Declaration.Function(
                name = nameToken.value.value,
                parameters = parameters,
                body = body,
                location = returnTypeToken.location,
            ).right()
        }

    private fun parseFunctionDeclarationParameters() = either {
        val paramOrVoidToken = peekToken()
        when (paramOrVoidToken?.value) {
            Token.Void -> {
                nextToken()
                emptyList()
            }
            Token.IntKeyword -> {
                val params = mutableListOf<AST.FunctionParameter>()
                while (true) {
                    expectToken(Token.IntKeyword).bind()
                    params.add(expectFunctionParameter().bind())

                    val peekToken = peekToken()
                    when (peekToken?.value) {
                        Token.Comma -> nextToken()
                        Token.CloseParen -> break
                        else -> raise(
                            ParserError(
                                "expected comma or close parenthesis in parameters list, " +
                                    "got ${peekToken?.value}",
                                peekToken?.location,
                            ),
                        )
                    }
                }
                params
            }
            else -> raise(ParserError("expected void or int, got ${paramOrVoidToken?.value}", paramOrVoidToken?.location))
        }
    }

    private fun parseBlock(): Either<ParserError, AST.Block> =
        either {
            expectToken(Token.OpenBrace).bind()
            val body = mutableListOf<AST.BlockItem>()
            while (peekToken()?.value != Token.CloseBrace) {
                body.add(parseBlockItem().bind())
            }
            expectToken(Token.CloseBrace).bind()
            AST.Block(body)
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
            when (peekToken()?.value) {
                Token.Semicolon -> {
                    nextToken()
                    AST.Declaration.Variable(nameToken.value.value, null, typeToken.location)
                }
                Token.Equal -> {
                    nextToken()
                    val initializer = parseExpression(0).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Declaration.Variable(nameToken.value.value, initializer, typeToken.location)
                }
                Token.OpenParen -> {
                    nextToken()
                    val parameters = parseFunctionDeclarationParameters().bind()
                    expectToken(Token.CloseParen).bind()
                    if (peekToken()?.value == Token.OpenBrace) {
                        val body = parseBlock().bind()
                        AST.Declaration.Function(nameToken.value.value, parameters, body, typeToken.location)
                    } else {
                        expectToken(Token.Semicolon).bind()
                        AST.Declaration.Function(nameToken.value.value, parameters, null, typeToken.location)
                    }
                }
                else -> {
                    raise(ParserError("expected = or (, got ${peekToken()?.value}", peekToken()?.location))
                }
            }
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
                Token.OpenBrace -> {
                    val block = parseBlock().bind()
                    AST.Statement.Compound(block)
                }
                Token.Do -> {
                    val doToken = expectToken(Token.Do).bind()
                    val statement = parseStatement().bind()
                    expectToken(Token.While).bind()
                    expectToken(Token.OpenParen).bind()
                    val condition = parseExpression(0).bind()
                    expectToken(Token.CloseParen).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.DoWhile(
                        body = statement,
                        condition = condition,
                        loopId = null,
                        location = doToken.location,
                    )
                }
                Token.While -> {
                    val whileToken = expectToken(Token.While).bind()
                    expectToken(Token.OpenParen).bind()
                    val condition = parseExpression(0).bind()
                    expectToken(Token.CloseParen).bind()
                    val body = parseStatement().bind()
                    AST.Statement.While(
                        condition = condition,
                        body = body,
                        loopId = null,
                        location = whileToken.location,
                    )
                }
                Token.For -> {
                    val forToken = expectToken(Token.For).bind()
                    expectToken(Token.OpenParen).bind()
                    val initializer = when (peekToken()?.value) {
                        Token.IntKeyword -> when (val declaration = parseDeclaration().bind()) {
                            is AST.Declaration.Variable -> AST.ForInitializer.Declaration(declaration)
                            is AST.Declaration.Function -> raise(
                                ParserError(
                                    "expected variable declaration in for initializer, got function declaration '${declaration.name}'",
                                    declaration.location,
                                ),
                            )
                        }
                        else -> parseOptionalExpression(Token.Semicolon).bind()?.let(AST.ForInitializer::Expression)
                    }
                    val condition =
                        parseOptionalExpression(Token.Semicolon).bind()
                    val post =
                        parseOptionalExpression(Token.CloseParen).bind()
                    val body = parseStatement().bind()
                    AST.Statement.For(
                        initializer = initializer,
                        condition = condition,
                        post = post,
                        body = body,
                        loopId = null,
                        location = forToken.location,
                    )
                }
                Token.Break -> {
                    val breakToken = expectToken(Token.Break).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Break(breakToken.location)
                }
                Token.Continue -> {
                    val continueToken = expectToken(Token.Continue).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Continue(loopId = null, continueToken.location)
                }
                Token.Switch -> {
                    val switchToken = expectToken(Token.Switch).bind()
                    expectToken(Token.OpenParen).bind()
                    val expression = parseExpression(0).bind()
                    expectToken(Token.CloseParen).bind()
                    val body = parseStatement().bind()
                    AST.Statement.Switch(
                        expression = expression,
                        body = body,
                        location = switchToken.location,
                        switchId = null,
                        caseExpressions = null,
                        hasDefault = false,
                    )
                }
                Token.Case -> {
                    val caseToken = expectToken(Token.Case).bind()
                    val expression = parseExpression(0).bind()
                    expectToken(Token.Colon).bind()
                    val body = parseStatement().bind()
                    AST.Statement.Case(
                        expression = expression,
                        body = body,
                        location = caseToken.location,
                        switchId = null,
                        caseId = null,
                    )
                }
                Token.Default -> {
                    val defaultToken = expectToken(Token.Default).bind()
                    expectToken(Token.Colon).bind()
                    val body = parseStatement().bind()
                    AST.Statement.Default(
                        body = body,
                        location = defaultToken.location,
                        switchId = null,
                    )
                }
                else -> {
                    val expression = parseExpression(0).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Statement.Expression(expression)
                }
            }
        }

    private fun parseOptionalExpression(endToken: Token): Either<ParserError, AST.Expression?> =
        either {
            val expression = if (peekToken()?.value == endToken) {
                null
            } else {
                parseExpression(0).bind()
            }
            expectToken(endToken).bind()
            expression
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
                        if (peekToken()?.value == Token.OpenParen) {
                            nextToken()
                            val arguments = parseFunctionCallArguments().bind()
                            expectToken(Token.CloseParen).bind()
                            AST.Expression.FunctionCall(
                                name = peekTokenValue.value,
                                arguments = arguments,
                                location = peekToken.location,
                            )
                        } else {
                            AST.Expression.Variable(peekTokenValue.value, peekToken.location)
                        }
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

    private fun parseFunctionCallArguments() = either<ParserError, List<AST.Expression>> {
        val arguments = mutableListOf<AST.Expression>()
        if (peekToken()?.value == Token.CloseParen) {
            return@either emptyList()
        }
        arguments.add(parseExpression(0).bind())
        while (peekToken()?.value != Token.CloseParen) {
            expectToken(Token.Comma).bind()
            arguments.add(parseExpression(0).bind())
        }
        arguments
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
