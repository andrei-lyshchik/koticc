package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.raise.ensureNotNull
import koticc.ast.AST
import koticc.ast.Type
import koticc.ast.convertTo
import koticc.token.Location

internal data class TypecheckedProgram(
    val value: AST.Program,
    val symbolTable: SymbolTable,
)

internal class Typechecker(private val nameMapping: Map<String, String>) {
    private val symbolTable: MutableMap<String, SymbolWithLocation> = mutableMapOf()

    private fun originalIdentifierName(name: String): String = nameMapping[name]
        ?: name

    fun typecheck(program: AST.Program): Either<SemanticAnalysisError, TypecheckedProgram> = either {
        val programWithTypes = program.copy(
            declarations = program.declarations.map { declaration ->
                when (declaration) {
                    is AST.Declaration.Function -> typecheckFunctionDeclaration(declaration).bind()
                    is AST.Declaration.Variable -> typecheckFileScopeVariableDeclaration(declaration).bind()
                }
            },
        )
        val symbolTable = symbolTable.mapValues { (_, symbolWithLocation) -> symbolWithLocation.value }
        TypecheckedProgram(programWithTypes, symbolTable)
    }

    private fun typecheckDeclaration(
        declaration: AST.Declaration,
    ): Either<SemanticAnalysisError, AST.Declaration> = either {
        when (declaration) {
            is AST.Declaration.Function -> typecheckFunctionDeclaration(declaration).bind()
            is AST.Declaration.Variable -> typecheckVariableDeclaration(declaration).bind()
        }
    }

    private fun typecheckFunctionDeclaration(
        functionDeclaration: AST.Declaration.Function,
    ): Either<SemanticAnalysisError, AST.Declaration.Function> = either {
        val global = functionDeclaration.storageClass != AST.StorageClass.Static
        val existingSymbol = symbolTable[functionDeclaration.name]
        val (previouslyDefined, previouslyGlobal) = if (existingSymbol != null) {
            when (val symbolValue = existingSymbol.value) {
                is Symbol.Function -> {
                    val bothDefined = symbolValue.defined && functionDeclaration.body != null
                    val conflictingTypes = symbolValue.type != functionDeclaration.type
                    if (bothDefined || conflictingTypes) {
                        raise(
                            SemanticAnalysisError(
                                "conflicting declaration of '${originalIdentifierName(functionDeclaration.name)}' " +
                                    "at ${existingSymbol.location.toDisplayString()}",
                                functionDeclaration.location,
                            ),
                        )
                    }
                    if (symbolValue.global && functionDeclaration.storageClass == AST.StorageClass.Static) {
                        raise(
                            SemanticAnalysisError(
                                "static function '${originalIdentifierName(functionDeclaration.name)}' declaration " +
                                    "follows non-static at ${existingSymbol.location.toDisplayString()}",
                                functionDeclaration.location,
                            ),
                        )
                    }
                    symbolValue.defined to symbolValue.global
                }
                is Symbol.Variable -> raise(
                    SemanticAnalysisError(
                        "variable '${originalIdentifierName(functionDeclaration.name)}' redeclared as a function",
                        functionDeclaration.location,
                    ),
                )
            }
        } else {
            false to null
        }
        symbolTable[functionDeclaration.name] = SymbolWithLocation(
            value = Symbol.Function(
                type = functionDeclaration.type,
                defined = functionDeclaration.body != null || previouslyDefined,
                global = previouslyGlobal ?: global,
            ),
            location = functionDeclaration.location,
        )
        functionDeclaration.copy(
            body = functionDeclaration.body?.let {
                functionDeclaration.parameters
                    .zip(functionDeclaration.type.parameters)
                    .forEach { (parameter, parameterType) ->
                        symbolTable[parameter.name] = SymbolWithLocation(
                            value = Symbol.Variable(type = parameterType, attributes = VariableAttributes.Local),
                            location = functionDeclaration.location,
                        )
                    }
                typecheckBlock(it, functionDeclaration.type).bind()
            },
        )
    }

    private fun typecheckFileScopeVariableDeclaration(
        declaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> = either {
        val convertedInitializer = convertDeclarationInitializer(declaration).bind()
        var initialValue = if (convertedInitializer != null) {
            InitialValue.Constant(convertedInitializer.value.toInitialValue())
        } else {
            when (declaration.storageClass) {
                AST.StorageClass.Extern -> InitialValue.NoInitializer
                AST.StorageClass.Static -> InitialValue.Tentative
                null -> InitialValue.Tentative
            }
        }
        var global = declaration.storageClass != AST.StorageClass.Static

        val existingSymbol = symbolTable[declaration.name]
        if (existingSymbol != null) {
            val existingVariable = when (existingSymbol.value) {
                is Symbol.Variable -> existingSymbol.value
                is Symbol.Function -> raise(
                    SemanticAnalysisError(
                        "function '${originalIdentifierName(declaration.name)}' redeclared as a variable",
                        declaration.location,
                    ),
                )
            }
            ensure(existingVariable.type == declaration.type) {
                SemanticAnalysisError(
                    "conflicting types for '${originalIdentifierName(declaration.name)}' " +
                        "at ${existingSymbol.location.toDisplayString()}",
                    declaration.location,
                )
            }
            if (declaration.storageClass == AST.StorageClass.Extern) {
                global = existingVariable.attributes.global
            } else if (existingVariable.attributes.global != global) {
                raise(
                    SemanticAnalysisError(
                        "static variable '${originalIdentifierName(declaration.name)}' declaration " +
                            "follows non-static at ${existingSymbol.location.toDisplayString()}",
                        declaration.location,
                    ),
                )
            }

            when (val existingInitialValue = existingVariable.attributes.initialValue) {
                is InitialValue.Constant -> {
                    if (initialValue is InitialValue.Constant) {
                        raise(
                            SemanticAnalysisError(
                                "redeclaration of '${originalIdentifierName(declaration.name)}' with a different initializer",
                                declaration.location,
                            ),
                        )
                    } else {
                        initialValue = existingInitialValue
                    }
                }
                is InitialValue.Tentative -> {
                    if (initialValue !is InitialValue.Constant) {
                        initialValue = InitialValue.Tentative
                    }
                }
                else -> {}
            }
        }

        symbolTable[declaration.name] = SymbolWithLocation(
            value = Symbol.Variable(
                type = declaration.type,
                attributes = VariableAttributes.Static(
                    initialValue = initialValue,
                    global = global,
                ),
            ),
            location = declaration.location,
        )
        declaration.copy(
            initializer = convertedInitializer,
        )
    }

    private fun typecheckBlock(block: AST.Block, currentFunctionType: Type.Function): Either<SemanticAnalysisError, AST.Block> = either {
        block.copy(
            blockItems = block.blockItems.map { blockItem ->
                when (blockItem) {
                    is AST.BlockItem.Declaration -> blockItem.copy(
                        declaration = typecheckDeclaration(blockItem.declaration).bind(),
                    )
                    is AST.BlockItem.Statement -> blockItem.copy(
                        statement = typecheckStatement(blockItem.statement, currentFunctionType).bind(),
                    )
                }
            },
        )
    }

    private fun typecheckVariableDeclaration(
        variableDeclaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> = either {
        when (variableDeclaration.storageClass) {
            AST.StorageClass.Extern -> {
                if (variableDeclaration.initializer != null) {
                    raise(
                        SemanticAnalysisError(
                            "extern local variable '${variableDeclaration.name}' cannot have an initializer",
                            variableDeclaration.location,
                        ),
                    )
                }
                val existingSymbol = symbolTable[variableDeclaration.name]
                if (existingSymbol != null) {
                    when (val symbolValue = existingSymbol.value) {
                        is Symbol.Function -> raise(
                            SemanticAnalysisError(
                                "function '${originalIdentifierName(variableDeclaration.name)}' redeclared as a variable",
                                variableDeclaration.location,
                            ),
                        )
                        is Symbol.Variable -> {
                            ensure(symbolValue.type == variableDeclaration.type) {
                                SemanticAnalysisError(
                                    "conflicting types for '${originalIdentifierName(variableDeclaration.name)}' " +
                                        "at ${existingSymbol.location.toDisplayString()}",
                                    variableDeclaration.location,
                                )
                            }
                        }
                    }
                } else {
                    symbolTable[variableDeclaration.name] = SymbolWithLocation(
                        value = Symbol.Variable(
                            type = variableDeclaration.type,
                            attributes = VariableAttributes.Static(
                                initialValue = InitialValue.NoInitializer,
                                global = true,
                            ),
                        ),
                        location = variableDeclaration.location,
                    )
                }
                variableDeclaration
            }
            AST.StorageClass.Static -> {
                val convertedInitializer = convertDeclarationInitializer(variableDeclaration).bind()
                val initialConstantValue = convertedInitializer?.value?.toInitialValue()
                    ?: variableDeclaration.type.toZeroInitialValue()
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(
                        type = variableDeclaration.type,
                        attributes = VariableAttributes.Static(
                            initialValue = InitialValue.Constant(initialConstantValue),
                            global = false,
                        ),
                    ),
                    location = variableDeclaration.location,
                )
                variableDeclaration.copy(
                    initializer = convertedInitializer,
                )
            }
            null -> {
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(type = variableDeclaration.type, attributes = VariableAttributes.Local),
                    location = variableDeclaration.location,
                )
                variableDeclaration.copy(
                    initializer = variableDeclaration.initializer?.let { typecheckExpression(it).bind().convertByAssignmentTo(variableDeclaration.type).bind() },
                )
            }
        }
    }

    private fun convertDeclarationInitializer(variableDeclaration: AST.Declaration.Variable): Either<SemanticAnalysisError, AST.Expression.Constant?> = either {
        when (variableDeclaration.initializer) {
            is AST.Expression.Constant -> {
                if (variableDeclaration.type is Type.Pointer && !variableDeclaration.initializer.isNullPointerConstant()) {
                    raise(
                        SemanticAnalysisError("invalid initializer '${variableDeclaration.initializer.toDisplayString()}' for variable '${originalIdentifierName(variableDeclaration.name)}'", variableDeclaration.location),
                    )
                }
                variableDeclaration.initializer.convertTo(variableDeclaration.type)
            }
            null -> null
            else -> raise(
                SemanticAnalysisError(
                    "non-constant initializer for variable '${originalIdentifierName(variableDeclaration.name)}'",
                    variableDeclaration.location,
                ),
            )
        }
    }

    private fun typecheckStatement(statement: AST.Statement, currentFunctionType: Type.Function): Either<SemanticAnalysisError, AST.Statement> = either {
        when (statement) {
            is AST.Statement.Break -> statement
            is AST.Statement.BreakLoop -> statement
            is AST.Statement.BreakSwitch -> statement
            is AST.Statement.Case -> statement.copy(
                expression = typecheckExpression(statement.expression).bind(),
                body = typecheckStatement(statement.body, currentFunctionType).bind(),
            )
            is AST.Statement.Compound -> statement.copy(block = typecheckBlock(statement.block, currentFunctionType).bind())
            is AST.Statement.Continue -> statement
            is AST.Statement.Default -> statement.copy(
                body = typecheckStatement(statement.body, currentFunctionType).bind(),
            )
            is AST.Statement.DoWhile -> {
                statement.copy(
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                    condition = typecheckExpression(statement.condition).bind(),
                )
            }
            is AST.Statement.Expression -> statement.copy(
                expression = typecheckExpression(statement.expression).bind(),
            )
            is AST.Statement.For -> {
                statement.copy(
                    initializer = statement.initializer?.let { initializer ->
                        when (initializer) {
                            is AST.ForInitializer.Declaration -> {
                                ensure(initializer.declaration.storageClass == null) {
                                    SemanticAnalysisError(
                                        "can't use storage class specifier in for loop initializer",
                                        initializer.declaration.location,
                                    )
                                }
                                initializer.copy(
                                    declaration = typecheckVariableDeclaration(initializer.declaration).bind(),
                                )
                            }
                            is AST.ForInitializer.Expression -> initializer.copy(
                                expression = typecheckExpression(initializer.expression).bind(),
                            )
                        }
                    },
                    condition = statement.condition?.let { typecheckExpression(it).bind() },
                    post = statement.post?.let { typecheckExpression(it).bind() },
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
            is AST.Statement.Goto -> statement
            is AST.Statement.If -> {
                statement.copy(
                    condition = typecheckExpression(statement.condition).bind(),
                    thenStatement = typecheckStatement(statement.thenStatement, currentFunctionType).bind(),
                    elseStatement = statement.elseStatement?.let { typecheckStatement(it, currentFunctionType).bind() },
                )
            }
            is AST.Statement.Labeled -> statement.copy(
                statement = typecheckStatement(statement.statement, currentFunctionType).bind(),
            )
            is AST.Statement.Null -> statement
            is AST.Statement.Return -> {
                statement.copy(
                    expression = typecheckExpression(statement.expression).bind().convertByAssignmentTo(currentFunctionType.returnType).bind(),
                )
            }
            is AST.Statement.Switch -> {
                val expression = typecheckExpression(statement.expression).bind()
                if (expression.resolvedType() is Type.Pointer) {
                    raise(
                        SemanticAnalysisError("invalid type ${expression.resolvedType().toDisplayString()} for switch controlling expression", expression.location),
                    )
                }
                statement.copy(
                    expression = typecheckExpression(statement.expression).bind(),
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
            is AST.Statement.While -> {
                statement.copy(
                    condition = typecheckExpression(statement.condition).bind(),
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
        }
    }

    @Suppress("KotlinConstantConditions")
    private fun typecheckExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> = either {
        when (expression) {
            is AST.Expression.Assignment -> {
                val left = typecheckExpression(expression.left).bind()
                if (!left.isLValue()) {
                    raise(SemanticAnalysisError("left side of assignment must be a left-value, got '${left.toDisplayString()}'", left.location))
                }
                val right = typecheckExpression(expression.right).bind().convertByAssignmentTo(left.resolvedType()).bind()
                expression.copy(
                    left = left,
                    right = right,
                ).ofType(left.resolvedType())
            }
            is AST.Expression.CompoundAssignment -> {
                val left = typecheckExpression(expression.left).bind()
                if (!left.isLValue()) {
                    raise(SemanticAnalysisError("left side of assignment must be a left-value, got '${left.toDisplayString()}'", left.location))
                }
                val right = typecheckExpression(expression.right).bind()
                val typecheckResult = typecheckBinary(left, right, expression.operator, expression.location).bind()
                val intermediateExpression = expression.copy(
                    left = left,
                    right = typecheckResult.right,
                    intermediateLeftType = typecheckResult.left.resolvedType(),
                    intermediateResultType = typecheckResult.resultType,
                ).ofType(typecheckResult.resultType)
                // just to run the validations
                intermediateExpression.convertByAssignmentTo(left.resolvedType())
                intermediateExpression.ofType(left.resolvedType())
            }
            is AST.Expression.Binary -> {
                val left = typecheckExpression(expression.left).bind()
                val right = typecheckExpression(expression.right).bind()
                val typecheckResult = typecheckBinary(left, right, expression.operator, expression.location).bind()
                expression.copy(
                    left = typecheckResult.left,
                    right = typecheckResult.right,
                ).ofType(typecheckResult.resultType)
            }
            is AST.Expression.Conditional -> {
                val thenExpression = typecheckExpression(expression.thenExpression).bind()
                val elseExpression = typecheckExpression(expression.elseExpression).bind()
                val commonType = getCommonType(thenExpression.resolvedType(), elseExpression.resolvedType())
                expression.copy(
                    condition = typecheckExpression(expression.condition).bind(),
                    thenExpression = thenExpression.castTo(commonType),
                    elseExpression = elseExpression.castTo(commonType),
                ).ofType(commonType)
            }
            is AST.Expression.FunctionCall -> {
                val functionType = when (val symbol = symbolTable[expression.name]?.value) {
                    is Symbol.Function -> symbol.type
                    else -> raise(
                        SemanticAnalysisError(
                            "'${originalIdentifierName(expression.name)}' is not a function",
                            expression.location,
                        ),
                    )
                }
                ensure(functionType.parameters.size == expression.arguments.size) {
                    SemanticAnalysisError(
                        "function '${originalIdentifierName(expression.name)}' expects " +
                            "${functionType.parameters.size} arguments, but ${expression.arguments.size} were provided",
                        expression.location,
                    )
                }
                val typecheckedArguments = expression.arguments.zip(functionType.parameters)
                    .map { (argument, parameterType) ->
                        typecheckExpression(argument).bind().convertByAssignmentTo(parameterType).bind()
                    }
                expression.copy(
                    arguments = typecheckedArguments,
                ).ofType(functionType.returnType)
            }
            is AST.Expression.Constant -> {
                expression.ofType(expression.value.type)
            }
            is AST.Expression.Postfix -> {
                val operand = typecheckExpression(expression.operand).bind()
                if (!operand.isLValue()) {
                    raise(
                        SemanticAnalysisError("operand of postfix expression must be a lvalue, got ${operand.toDisplayString()}", expression.location),
                    )
                }
                expression.copy(
                    operand = operand,
                ).ofType(operand.resolvedType())
            }
            is AST.Expression.Unary -> {
                val operand = typecheckExpression(expression.operand).bind()
                if (expression.operator != AST.UnaryOperator.LogicalNegate && operand.resolvedType() is Type.Pointer) {
                    raise(SemanticAnalysisError("invalid operand type for '${expression.operator.toDisplayString()}': ${operand.resolvedType().toDisplayString()}", expression.location))
                }
                val type = when (expression.operator) {
                    AST.UnaryOperator.Negate -> operand.resolvedType()
                    AST.UnaryOperator.Complement -> {
                        ensure(operand.resolvedType() !is Type.Double) {
                            SemanticAnalysisError("invalid operand type for '~': double", expression.location)
                        }
                        operand.resolvedType()
                    }
                    AST.UnaryOperator.LogicalNegate -> Type.Int
                }
                expression.copy(
                    operand = operand,
                ).ofType(type)
            }
            is AST.Expression.Variable -> {
                val symbol = symbolTable[expression.name]
                ensureNotNull(symbol) {
                    SemanticAnalysisError("undeclared variable '${originalIdentifierName(expression.name)}'", expression.location)
                }
                ensure(symbol.value is Symbol.Variable) {
                    SemanticAnalysisError("'${originalIdentifierName(expression.name)}' is not a variable", expression.location)
                }
                expression.ofType(symbol.value.type)
            }

            is AST.Expression.Cast -> {
                val inner = typecheckExpression(expression.expression).bind()
                if (inner.resolvedType() is Type.Double &&
                    expression.targetType is Type.Pointer ||
                    inner.resolvedType() is Type.Pointer &&
                    expression.targetType is Type.Double
                ) {
                    raise(
                        SemanticAnalysisError("can't cast expression of type ${inner.resolvedType().toDisplayString()} to ${expression.targetType.toDisplayString()}", expression.location),
                    )
                }
                expression.copy(
                    expression = typecheckExpression(expression.expression).bind(),
                ).ofType(expression.targetType)
            }

            is AST.Expression.AddressOf -> {
                val typedInnerExpression = typecheckExpression(expression.expression).bind()
                val type = if (typedInnerExpression.isLValue()) {
                    Type.Pointer(referenced = typedInnerExpression.resolvedType())
                } else {
                    raise(SemanticAnalysisError("Can't take address of non lvalue, got ${typedInnerExpression.toDisplayString()}", typedInnerExpression.location))
                }
                expression.copy(
                    expression = typedInnerExpression,
                ).ofType(type)
            }
            is AST.Expression.Dereference -> {
                val typedInnerExpression = typecheckExpression(expression.expression).bind()
                val type = when (val innerType = typedInnerExpression.resolvedType()) {
                    is Type.Pointer -> innerType.referenced
                    else -> raise(
                        SemanticAnalysisError(
                            "Expected pointer for dereference, " +
                                "got ${typedInnerExpression.toDisplayString()}",
                            typedInnerExpression.location,
                        ),
                    )
                }
                expression.copy(
                    expression = typedInnerExpression,
                ).ofType(type)
            }
        }
    }

    @Suppress("KotlinConstantConditions")
    private fun typecheckBinary(
        left: AST.Expression,
        right: AST.Expression,
        operator: AST.BinaryOperator,
        location: Location,
    ): Either<SemanticAnalysisError, BinaryTypecheckResult> = either {
        if (operator in operatorsForbiddenForDouble &&
            (left.resolvedType() is Type.Double || right.resolvedType() is Type.Double)
        ) {
            raise(
                SemanticAnalysisError(
                    "invalid double operand type for '${operator.toDisplayString()}'",
                    location,
                ),
            )
        }

        if (operator !in operatorsAllowedForPointers &&
            (left.resolvedType() is Type.Pointer || right.resolvedType() is Type.Pointer)
        ) {
            raise(
                SemanticAnalysisError(
                    "invalid pointer type for '${operator.toDisplayString()}'",
                    location,
                ),
            )
        }

        if (operator == AST.BinaryOperator.LogicalAnd || operator == AST.BinaryOperator.LogicalOr) {
            return@either BinaryTypecheckResult(left = left, right = right, resultType = Type.Int)
        }

        if (operator == AST.BinaryOperator.ShiftLeft || operator == AST.BinaryOperator.ShiftRight) {
            return@either BinaryTypecheckResult(left = left, right = right, resultType = left.resolvedType())
        }

        val commonType = if (left.resolvedType() is Type.Pointer || right.resolvedType() is Type.Pointer) {
            getCommonPointerType(left, right).bind()
        } else {
            getCommonType(left.resolvedType(), right.resolvedType())
        }
        val resultType = when (operator) {
            AST.BinaryOperator.Add,
            AST.BinaryOperator.Subtract,
            AST.BinaryOperator.Multiply,
            AST.BinaryOperator.Divide,
            AST.BinaryOperator.Modulo,
            AST.BinaryOperator.BitwiseAnd,
            AST.BinaryOperator.BitwiseOr,
            AST.BinaryOperator.BitwiseXor,
            -> commonType
            AST.BinaryOperator.Equal,
            AST.BinaryOperator.NotEqual,
            AST.BinaryOperator.LessThan,
            AST.BinaryOperator.LessThanOrEqual,
            AST.BinaryOperator.GreaterThan,
            AST.BinaryOperator.GreaterThanOrEqual,
            -> Type.Int
            AST.BinaryOperator.ShiftLeft,
            AST.BinaryOperator.ShiftRight,
            AST.BinaryOperator.LogicalAnd,
            AST.BinaryOperator.LogicalOr,
            -> error("unreachable")
        }
        BinaryTypecheckResult(
            left = left.castTo(commonType),
            right = right.castTo(commonType),
            resultType = resultType,
        )
    }

    private fun AST.Expression.isLValue() = when (this) {
        is AST.Expression.Variable -> true
        is AST.Expression.Dereference -> true
        else -> false
    }

    private fun getCommonType(type1: Type.Data, type2: Type.Data): Type.Data = when {
        type1 == type2 -> type1
        type1 == Type.Double || type2 == Type.Double -> Type.Double
        type1.size() == type2.size() -> {
            if (type1.signed()) {
                type2
            } else {
                type1
            }
        }
        type1.size() > type2.size() -> type1
        else -> type2
    }

    private fun getCommonPointerType(e1: AST.Expression, e2: AST.Expression): Either<SemanticAnalysisError, Type.Data> = either {
        val type1 = e1.resolvedType()
        val type2 = e2.resolvedType()

        when {
            type1 == type2 -> type1
            e1.isNullPointerConstant() -> type2
            e2.isNullPointerConstant() -> type1
            else -> raise(SemanticAnalysisError("incompatible types: ${type1.toDisplayString()}, ${type2.toDisplayString()}", e1.location))
        }
    }

    private fun AST.Expression.isNullPointerConstant() = when (this) {
        is AST.Expression.Constant -> when (this.value) {
            AST.IntConstant(0) -> true
            AST.LongConstant(0L) -> true
            AST.UIntConstant(0u) -> true
            AST.ULongConstant(0uL) -> true
            else -> false
        }
        else -> false
    }

    private fun AST.Expression.convertByAssignmentTo(targetType: Type.Data): Either<SemanticAnalysisError, AST.Expression> = either {
        when {
            resolvedType() == targetType -> {
                this@convertByAssignmentTo
            }
            resolvedType() is Type.Arithmetic && targetType is Type.Arithmetic -> {
                castTo(targetType)
            }
            this@convertByAssignmentTo.isNullPointerConstant() && targetType is Type.Pointer -> {
                castTo(targetType)
            }
            else -> {
                raise(
                    SemanticAnalysisError(
                        "Cannot convert ${this@convertByAssignmentTo.toDisplayString()} to type ${targetType.toDisplayString()}",
                        this@convertByAssignmentTo.location,
                    ),
                )
            }
        }
    }

    private fun AST.Expression.castTo(targetType: Type.Data): AST.Expression {
        if (resolvedType() == targetType) {
            return this
        }

        return AST.Expression.Cast(
            expression = this,
            targetType = targetType,
            type = targetType,
            location = location,
        )
    }

    private data class SymbolWithLocation(
        val value: Symbol,
        val location: Location,
    )

    private data class BinaryTypecheckResult(
        val left: AST.Expression,
        val right: AST.Expression,
        val resultType: Type.Data,
    )

    companion object {
        private val operatorsForbiddenForDouble = setOf(
            AST.BinaryOperator.Modulo,
            AST.BinaryOperator.BitwiseXor,
            AST.BinaryOperator.BitwiseAnd,
            AST.BinaryOperator.BitwiseOr,
            AST.BinaryOperator.ShiftLeft,
            AST.BinaryOperator.ShiftRight,
        )

        private val operatorsAllowedForPointers = setOf(
            AST.BinaryOperator.Add,
            AST.BinaryOperator.Subtract,
            AST.BinaryOperator.Equal,
            AST.BinaryOperator.NotEqual,
            AST.BinaryOperator.LogicalAnd,
            AST.BinaryOperator.LogicalOr,
        )
    }
}
