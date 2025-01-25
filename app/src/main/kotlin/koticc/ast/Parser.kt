@file:Suppress("ktlint:standard:filename")

package koticc.ast

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.raise.ensureNotNull
import arrow.core.right
import koticc.CompilerError
import koticc.token.Location
import koticc.token.Token
import koticc.token.TokenWithLocation

data class ParserError(val message: String, val location: Location?) : CompilerError {
    override fun message(): String = "parser error, at ${location?.toDisplayString() ?: "EOF"}: $message"
}

fun parse(tokens: List<TokenWithLocation>): Either<ParserError, AST.Program> = Parser(tokens).parse()

private class Parser(
    val tokens: List<TokenWithLocation>,
) {
    var current = 0

    fun parse(): Either<ParserError, AST.Program> =
        either {
            val declarations = mutableListOf<AST.Declaration>()
            while (peekToken() != null) {
                declarations.add(parseDeclaration().bind())
            }
            AST.Program(declarations)
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
                ParserError("expected token ${expected.toDisplayString()}, but got EOF", null).left()
            } else {
                ParserError("expected token ${expected.toDisplayString()}, but got ${nextToken.value.toDisplayString()}", nextToken.location)
                    .left()
            }
        }
    }

    private fun expectIdentifier(): Either<ParserError, TokenIdentifierWithLocation> {
        val nextToken = nextToken()
        return when (val tokenValue = nextToken?.value) {
            is Token.Identifier -> TokenIdentifierWithLocation(tokenValue, nextToken.location).right()
            else ->
                ParserError("expected identifier, got ${nextToken?.value.toDisplayString()}", nextToken?.location)
                    .left()
        }
    }

    private fun expectFunctionParameter(): Either<ParserError, AST.FunctionParameter> =
        either {
            val nameToken = expectIdentifier().bind()
            AST.FunctionParameter(nameToken.value.value, nameToken.location)
        }

    private fun parseFunctionDeclarationParameters() = either {
        val paramOrVoidToken = peekToken()
        when {
            paramOrVoidToken?.value == Token.Void -> {
                nextToken()
                emptyList()
            }
            isDeclarationSpecifier(paramOrVoidToken?.value) -> {
                val params = mutableListOf<FunctionParameterWithType>()
                while (true) {
                    val type = parseType().bind()
                    params.add(FunctionParameterWithType(value = expectFunctionParameter().bind(), type = type))

                    val peekToken = peekToken()
                    when (peekToken?.value) {
                        Token.Comma -> nextToken()
                        Token.CloseParen -> break
                        else -> raise(
                            ParserError(
                                "expected comma or close parenthesis in parameters list, " +
                                    "got ${peekToken?.value.toDisplayString()}",
                                peekToken?.location,
                            ),
                        )
                    }
                }
                params
            }
            else -> raise(ParserError("expected 'void' or 'int', got ${paramOrVoidToken?.value.toDisplayString()}", paramOrVoidToken?.location))
        }
    }

    private data class FunctionParameterWithType(
        val value: AST.FunctionParameter,
        val type: Type.Data,
    )

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
            when {
                isDeclarationSpecifier(peekToken()?.value) -> {
                    AST.BlockItem.Declaration(parseDeclaration().bind())
                }
                else -> {
                    AST.BlockItem.Statement(parseStatement().bind())
                }
            }
        }

    private fun isDeclarationSpecifier(token: Token?): Boolean =
        token != null && toDeclarationSpecifierOrNull(token) != null

    private fun toDeclarationSpecifierOrNull(token: Token): DeclarationSpecifier? =
        when (token) {
            Token.IntKeyword -> DeclarationSpecifier.Type.Int
            Token.LongKeyword -> DeclarationSpecifier.Type.Long
            Token.Extern -> DeclarationSpecifier.StorageClass.Extern
            Token.Static -> DeclarationSpecifier.StorageClass.Static
            else -> null
        }

    private fun parseDeclaration(): Either<ParserError, AST.Declaration> =
        either {
            val declarationSpecifiers = parseDeclarationSpecifiers().bind()
            val nameToken = expectIdentifier().bind()
            when (peekToken()?.value) {
                Token.Semicolon -> {
                    nextToken()
                    AST.Declaration.Variable(
                        nameToken.value.value,
                        null,
                        declarationSpecifiers.type,
                        declarationSpecifiers.storageClass,
                        declarationSpecifiers.location,
                    )
                }
                Token.Equal -> {
                    nextToken()
                    val initializer = parseExpression(0).bind()
                    expectToken(Token.Semicolon).bind()
                    AST.Declaration.Variable(
                        nameToken.value.value,
                        initializer,
                        declarationSpecifiers.type,
                        declarationSpecifiers.storageClass,
                        declarationSpecifiers.location,
                    )
                }
                Token.OpenParen -> {
                    nextToken()
                    val parameters = parseFunctionDeclarationParameters().bind()
                    expectToken(Token.CloseParen).bind()
                    if (peekToken()?.value == Token.OpenBrace) {
                        val body = parseBlock().bind()
                        AST.Declaration.Function(
                            nameToken.value.value,
                            parameters.map { it.value },
                            body,
                            Type.Function(parameters = parameters.map { it.type }, returnType = declarationSpecifiers.type),
                            declarationSpecifiers.storageClass,
                            declarationSpecifiers.location,
                        )
                    } else {
                        expectToken(Token.Semicolon).bind()
                        AST.Declaration.Function(
                            nameToken.value.value,
                            parameters.map { it.value },
                            null,
                            Type.Function(parameters = parameters.map { it.type }, returnType = declarationSpecifiers.type),
                            declarationSpecifiers.storageClass,
                            declarationSpecifiers.location,
                        )
                    }
                }
                else -> {
                    raise(ParserError("expected '=' or '(', got ${peekToken()?.value.toDisplayString()}", peekToken()?.location))
                }
            }
        }

    private fun parseDeclarationSpecifiers(): Either<ParserError, DeclarationSpecifiers> = either {
        val specifiers = mutableListOf<DeclarationSpecifier>()
        val startLocation = ensureNotNull(peekToken()?.location) {
            ParserError("expected declaration specifier, got EOF", null)
        }
        while (true) {
            val specifier = peekToken()?.value?.let(::toDeclarationSpecifierOrNull)
            if (specifier == null) {
                break
            }
            specifiers.add(specifier)
            nextToken()
        }
        val typeSpecifiers = specifiers.filterIsInstance<DeclarationSpecifier.Type>()
        val type = getType(typeSpecifiers, startLocation).bind()

        val storageClasses = specifiers.filterIsInstance<DeclarationSpecifier.StorageClass>()
        ensure(storageClasses.size <= 1) {
            raise(ParserError("invalid storage class specifier", startLocation))
        }

        DeclarationSpecifiers(
            type = type,
            storageClass = storageClasses.firstOrNull()?.toASTStorageClass(),
            location = startLocation,
        )
    }

    private fun parseType(): Either<ParserError, Type.Data> = either {
        val declarationSpecifiers = parseDeclarationSpecifiers().bind()
        ensure(declarationSpecifiers.storageClass == null) {
            ParserError("storage class specifier: ${declarationSpecifiers.storageClass?.toDisplayString()} not allowed in type", declarationSpecifiers.location)
        }
        declarationSpecifiers.type
    }

    private fun getType(typeSpecifiers: List<DeclarationSpecifier.Type>, startLocation: Location): Either<ParserError, Type.Data> = either {
        if (typeSpecifiers == listOf(DeclarationSpecifier.Type.Int)) {
            return@either Type.Int
        }
        if (typeSpecifiers == listOf(DeclarationSpecifier.Type.Long) ||
            typeSpecifiers == listOf(DeclarationSpecifier.Type.Long, DeclarationSpecifier.Type.Int) ||
            typeSpecifiers == listOf(DeclarationSpecifier.Type.Int, DeclarationSpecifier.Type.Long)
        ) {
            return@either Type.Long
        }
        val typeSpecifierString = typeSpecifiers.joinToString(separator = " ")
        raise(
            ParserError("invalid type specifier: '$typeSpecifierString'", startLocation),
        )
    }

    private fun DeclarationSpecifier.StorageClass.toASTStorageClass() =
        when (this) {
            DeclarationSpecifier.StorageClass.Extern -> AST.StorageClass.Extern
            DeclarationSpecifier.StorageClass.Static -> AST.StorageClass.Static
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
                    val initializer = when {
                        isDeclarationSpecifier(peekToken()?.value) -> when (val declaration = parseDeclaration().bind()) {
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
                                type = null,
                            )
                        is BinaryOperatorLike.Assignment ->
                            AST.Expression.Assignment(
                                left = left,
                                // minPrecedence = precedence => right associative
                                right = parseExpression(precedence).bind(),
                                type = null,
                            )
                        is BinaryOperatorLike.CompoundAssignmentOperator ->
                            compoundAssignment(
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
                                type = null,
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
                        AST.Expression.Constant(AST.IntConstant(peekTokenValue.value), null, peekToken.location)
                    }
                    is Token.LongLiteral -> {
                        nextToken()
                        AST.Expression.Constant(AST.LongConstant(peekTokenValue.value), null, peekToken.location)
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
                                type = null,
                            )
                        } else {
                            AST.Expression.Variable(peekTokenValue.value, null, peekToken.location)
                        }
                    }
                    is Token.OpenParen -> {
                        val closeParen = expectToken(Token.OpenParen).bind()
                        val possibleTypeSpecifier = peekToken()
                        if (isDeclarationSpecifier(possibleTypeSpecifier?.value)) {
                            val targetType = parseType().bind()
                            expectToken(Token.CloseParen).bind()
                            val expression = parseFactor().bind()
                            AST.Expression.Cast(
                                targetType = targetType,
                                expression = expression,
                                type = null,
                                location = closeParen.location,
                            )
                        } else {
                            val expression = parseExpression(0).bind()
                            expectToken(Token.CloseParen).bind()
                            expression
                        }
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
                    else -> raise(ParserError("expected factor, got ${peekTokenValue.toDisplayString()}", peekToken?.location))
                }
            var postfixOperator = peekToken()?.value?.toPostfixOperatorOrNull()
            while (postfixOperator != null) {
                nextToken()
                factor =
                    AST.Expression.Postfix(
                        operator = postfixOperator,
                        operand = factor,
                        type = null,
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
                    Token.DoublePlus -> AST.BinaryOperator.Add
                    Token.DoubleMinus -> AST.BinaryOperator.Subtract
                    else ->
                        raise(
                            ParserError(
                                "expected prefix increment or decrement, got ${peekToken?.value.toDisplayString()}",
                                peekToken?.location,
                            ),
                        )
                }
            nextToken()
            val operand = parseFactor().bind()
            compoundAssignment(
                operator = operator,
                left = operand,
                right = AST.Expression.Constant(AST.IntConstant(1), null, peekToken.location),
            )
        }

    private fun compoundAssignment(
        left: AST.Expression,
        right: AST.Expression,
        operator: AST.BinaryOperator,
    ) = AST.Expression.Assignment(
        left = left,
        right = AST.Expression.Binary(
            operator = operator,
            left = left,
            right = right,
            type = null,
        ),
        type = null,
    )

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
                                "expected unary operator, got ${peekToken?.value.toDisplayString()}",
                                peekToken?.location,
                            ),
                        )
                }
            nextToken()
            val operand = parseFactor().bind()
            AST.Expression.Unary(unaryOperator, operand, null, peekToken.location)
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
                    AST.BinaryOperator.Add,
                    token.location,
                )
            Token.MinusEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.Subtract,
                    token.location,
                )
            Token.AsteriskEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.Multiply,
                    token.location,
                )
            Token.SlashEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.Divide,
                    token.location,
                )
            Token.PercentEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.Modulo,
                    token.location,
                )
            Token.AmpersandEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.BitwiseAnd,
                    token.location,
                )
            Token.CaretEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.BitwiseXor,
                    token.location,
                )
            Token.PipeEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.BitwiseOr,
                    token.location,
                )
            Token.DoubleLessThanEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.ShiftLeft,
                    token.location,
                )
            Token.DoubleGreaterThanEqual ->
                BinaryOperatorLike.CompoundAssignmentOperator(
                    AST.BinaryOperator.ShiftRight,
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

    private fun Token?.toDisplayString() = if (this != null) {
        "'${this.toDisplayString()}'"
    } else {
        "EOF"
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
        val value: AST.BinaryOperator,
        val location: Location,
    ) : BinaryOperatorLike

    data object Conditional : BinaryOperatorLike
}

private sealed interface DeclarationSpecifier {
    sealed interface Type : DeclarationSpecifier {
        data object Int : Type
        data object Long : Type
    }
    sealed interface StorageClass : DeclarationSpecifier {
        data object Extern : StorageClass
        data object Static : StorageClass
    }
}

// type is only int for now
private data class DeclarationSpecifiers(
    val type: Type.Data,
    val storageClass: AST.StorageClass?,
    val location: Location,
)
