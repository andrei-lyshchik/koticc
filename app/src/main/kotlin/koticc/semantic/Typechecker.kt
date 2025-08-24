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
        if (functionDeclaration.type.returnType is Type.Array) {
            raise(
                SemanticAnalysisError(
                    "function '${originalIdentifierName(functionDeclaration.name)}' cannot return an array",
                    functionDeclaration.location,
                ),
            )
        }
        val adjustedType = functionDeclaration.type.copy(
            parameters = functionDeclaration.type.parameters.map { parameterType ->
                when (parameterType) {
                    is Type.Array -> Type.Pointer(parameterType.elementType)
                    else -> parameterType
                }
            },
        )
        symbolTable[functionDeclaration.name] = SymbolWithLocation(
            value = Symbol.Function(
                type = adjustedType,
                defined = functionDeclaration.body != null || previouslyDefined,
                global = previouslyGlobal ?: global,
            ),
            location = functionDeclaration.location,
        )
        functionDeclaration.copy(
            body = functionDeclaration.body?.let {
                functionDeclaration.parameters
                    .zip(adjustedType.parameters)
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
        val convertedInitializerConstant = convertStaticDeclarationInitializer(
            initializer = declaration.initializer,
            type = declaration.type,
            name = declaration.name,
            location = declaration.location,
        ).bind()
        var initialValue = if (convertedInitializerConstant.isNotEmpty()) {
            InitialValue.Constant(convertedInitializerConstant)
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
            initializer = null,
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
                val convertedInitializers = convertStaticDeclarationInitializer(
                    initializer = variableDeclaration.initializer,
                    type = variableDeclaration.type,
                    name = variableDeclaration.name,
                    location = variableDeclaration.location,
                ).bind()
                val initialConstantValue = convertedInitializers.takeIf { it.isNotEmpty() }
                    ?: listOf(variableDeclaration.type.toZeroInitialValue())
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
                    initializer = null,
                )
            }
            null -> {
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(type = variableDeclaration.type, attributes = VariableAttributes.Local),
                    location = variableDeclaration.location,
                )
                variableDeclaration.copy(
                    initializer = variableDeclaration.initializer?.let { initializer ->
                        typecheckVariableInitializer(initializer, variableDeclaration.type, variableDeclaration.location).bind()
                    },
                )
            }
        }
    }

    private fun typecheckVariableInitializer(
        initializer: AST.VariableInitializer,
        targetType: Type.Data,
        location: Location,
    ): Either<SemanticAnalysisError, AST.VariableInitializer> = either {
        when (initializer) {
            is AST.VariableInitializer.Compound -> {
                val elementType = when (targetType) {
                    is Type.Array -> targetType.elementType
                    else -> raise(SemanticAnalysisError("Can't initialize a scalar variable with a compound initializer", location))
                }
                if (initializer.initializers.size > targetType.size) {
                    raise(
                        SemanticAnalysisError(
                            "too many initializers for variable, expected ${targetType.size}, got ${initializer.initializers.size}",
                            location,
                        ),
                    )
                }
                val zeroPadding = if (initializer.initializers.size < targetType.size) {
                    List(targetType.size.toInt() - initializer.initializers.size) {
                        elementType.toZeroInitializer(location)
                    }
                } else {
                    emptyList()
                }

                AST.VariableInitializer.Compound(
                    initializers = initializer.initializers.map { initializer ->
                        typecheckVariableInitializer(initializer, elementType, location).bind()
                    } + zeroPadding,
                    type = targetType,
                )
            }
            is AST.VariableInitializer.Single -> AST.VariableInitializer.Single(
                expression = typecheckAndConvertExpression(initializer.expression).bind().convertByAssignmentTo(targetType).bind(),
                type = targetType,
            )
        }
    }

    private fun Type.Data.toZeroInitializer(location: Location): AST.VariableInitializer = when (this) {
        is Type.Int -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.IntConstant(0), type = this, location = location), this)
        is Type.UInt -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.UIntConstant(0u), type = this, location = location), this)
        is Type.Long -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.LongConstant(0L), type = this, location = location), this)
        is Type.ULong -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.ULongConstant(0uL), type = this, location = location), this)
        is Type.Double -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.DoubleConstant(0.0), type = this, location = location), this)
        is Type.Pointer -> AST.VariableInitializer.Single(AST.Expression.Constant(value = AST.IntConstant(0), type = this, location = location), this)
        is Type.Array -> AST.VariableInitializer.Compound(
            initializers = buildList {
                repeat(size) {
                    add(elementType.toZeroInitializer(location))
                }
            },
            type = this,
        )
    }

    private fun convertStaticDeclarationInitializer(
        initializer: AST.VariableInitializer?,
        type: Type.Data,
        name: String,
        location: Location,
    ): Either<SemanticAnalysisError, List<InitialConstantValue>> = either {
        when (initializer) {
            is AST.VariableInitializer.Compound -> {
                val (elementType, size) = when (type) {
                    is Type.Array -> type.elementType to type.size
                    else -> raise(
                        SemanticAnalysisError("Can't initialize a scalar variable with a compound initializer", location),
                    )
                }
                if (initializer.initializers.size > size) {
                    raise(
                        SemanticAnalysisError(
                            "too many initializers for variable '$name', expected $size, got ${initializer.initializers.size}",
                            location,
                        ),
                    )
                }
                val convertedSubInitializers = initializer.initializers.flatMap { subInitializer ->
                    convertStaticDeclarationInitializer(subInitializer, elementType, name, location).bind()
                }
                val zeroPadding = if (convertedSubInitializers.size < size) {
                    listOf(
                        InitialConstantValue.Zero(bytes = elementType.byteSize().toInt() * (size - convertedSubInitializers.size).toInt()),
                    )
                } else {
                    emptyList()
                }
                convertedSubInitializers + zeroPadding
            }
            is AST.VariableInitializer.Single -> {
                if (type is Type.Array) {
                    raise(
                        SemanticAnalysisError("Can't initialize an array with a scalar expression", location),
                    )
                }
                when (val expression = initializer.expression) {
                    is AST.Expression.Constant -> {
                        if (type is Type.Pointer && !expression.isNullPointerConstant()) {
                            raise(
                                SemanticAnalysisError(
                                    "invalid initializer '${expression.toDisplayString()}' " +
                                        "for variable '${originalIdentifierName(name)}'",
                                    location,
                                ),
                            )
                        }
                        listOf(expression.convertTo(type).value.toInitialValue())
                    }

                    else -> raise(
                        SemanticAnalysisError(
                            "non-constant initializer for variable '${originalIdentifierName(name)}'",
                            location,
                        ),
                    )
                }
            }
            null -> emptyList()
        }
    }

    private fun typecheckStatement(statement: AST.Statement, currentFunctionType: Type.Function): Either<SemanticAnalysisError, AST.Statement> = either {
        when (statement) {
            is AST.Statement.Break -> statement
            is AST.Statement.BreakLoop -> statement
            is AST.Statement.BreakSwitch -> statement
            is AST.Statement.Case -> statement.copy(
                expression = typecheckAndConvertExpression(statement.expression).bind(),
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
                    condition = typecheckAndConvertExpression(statement.condition).bind(),
                )
            }
            is AST.Statement.Expression -> statement.copy(
                expression = typecheckAndConvertExpression(statement.expression).bind(),
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
                                expression = typecheckAndConvertExpression(initializer.expression).bind(),
                            )
                        }
                    },
                    condition = statement.condition?.let { typecheckAndConvertExpression(it).bind() },
                    post = statement.post?.let { typecheckAndConvertExpression(it).bind() },
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
            is AST.Statement.Goto -> statement
            is AST.Statement.If -> {
                statement.copy(
                    condition = typecheckAndConvertExpression(statement.condition).bind(),
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
                    expression = typecheckAndConvertExpression(statement.expression).bind().convertByAssignmentTo(currentFunctionType.returnType).bind(),
                )
            }
            is AST.Statement.Switch -> {
                val expression = typecheckAndConvertExpression(statement.expression).bind()
                if (expression.resolvedType() is Type.Pointer) {
                    raise(
                        SemanticAnalysisError("invalid type ${expression.resolvedType().toDisplayString()} for switch controlling expression", expression.location),
                    )
                }
                statement.copy(
                    expression = typecheckAndConvertExpression(statement.expression).bind(),
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
            is AST.Statement.While -> {
                statement.copy(
                    condition = typecheckAndConvertExpression(statement.condition).bind(),
                    body = typecheckStatement(statement.body, currentFunctionType).bind(),
                )
            }
        }
    }

    private fun typecheckAndConvertExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> = either {
        val result = typecheckExpression(expression).bind()
        when (val type = result.resolvedType()) {
            is Type.Array -> {
                AST.Expression.AddressOf(
                    expression = result,
                    type = Type.Pointer(type.elementType),
                    location = result.location,
                )
            }
            else -> result
        }
    }

    private fun typecheckExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> = either {
        when (expression) {
            is AST.Expression.Assignment -> {
                val left = typecheckAndConvertExpression(expression.left).bind()
                if (!left.isLValue()) {
                    raise(SemanticAnalysisError("left side of assignment must be a left-value, got '${left.toDisplayString()}'", left.location))
                }
                val right = typecheckAndConvertExpression(expression.right).bind().convertByAssignmentTo(left.resolvedType()).bind()
                expression.copy(
                    left = left,
                    right = right,
                ).ofType(left.resolvedType())
            }
            is AST.Expression.CompoundAssignment -> {
                val left = typecheckAndConvertExpression(expression.left).bind()
                if (!left.isLValue()) {
                    raise(SemanticAnalysisError("left side of assignment must be a left-value, got '${left.toDisplayString()}'", left.location))
                }
                val right = typecheckAndConvertExpression(expression.right).bind()
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
                val left = typecheckAndConvertExpression(expression.left).bind()
                val right = typecheckAndConvertExpression(expression.right).bind()
                val typecheckResult = typecheckBinary(left, right, expression.operator, expression.location).bind()
                expression.copy(
                    left = typecheckResult.left,
                    right = typecheckResult.right,
                ).ofType(typecheckResult.resultType)
            }
            is AST.Expression.Conditional -> {
                val thenExpression = typecheckAndConvertExpression(expression.thenExpression).bind()
                val elseExpression = typecheckAndConvertExpression(expression.elseExpression).bind()
                val commonType = getCommonType(thenExpression.resolvedType(), elseExpression.resolvedType())
                expression.copy(
                    condition = typecheckAndConvertExpression(expression.condition).bind(),
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
                        typecheckAndConvertExpression(argument).bind().convertByAssignmentTo(parameterType).bind()
                    }
                expression.copy(
                    arguments = typecheckedArguments,
                ).ofType(functionType.returnType)
            }
            is AST.Expression.Constant -> {
                expression.ofType(expression.value.type)
            }
            is AST.Expression.Postfix -> {
                val operand = typecheckAndConvertExpression(expression.operand).bind()
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
                val operand = typecheckAndConvertExpression(expression.operand).bind()
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
                val inner = typecheckAndConvertExpression(expression.expression).bind()
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
                    expression = typecheckAndConvertExpression(expression.expression).bind(),
                ).ofType(expression.targetType)
            }

            is AST.Expression.AddressOf -> {
                val typedInnerExpression = typecheckAndConvertExpression(expression.expression).bind()
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
                val typedInnerExpression = typecheckAndConvertExpression(expression.expression).bind()
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

            is AST.Expression.Subscript -> {
                var typedExpression = typecheckAndConvertExpression(expression.expression).bind()
                var typedIndex = typecheckAndConvertExpression(expression.index).bind()
                val type = when {
                    typedExpression.resolvedType() is Type.Pointer && typedIndex.resolvedType().isInteger() -> {
                        val pointerType = (typedExpression.resolvedType() as Type.Pointer).referenced
                        typedIndex = typedIndex.castTo(Type.Long)
                        pointerType
                    }
                    typedExpression.resolvedType().isInteger() && typedIndex.resolvedType() is Type.Pointer -> {
                        val pointerType = (typedIndex.resolvedType() as Type.Pointer).referenced
                        typedExpression = typedExpression.castTo(Type.Long)
                        pointerType
                    }
                    else -> {
                        raise(
                            SemanticAnalysisError(
                                "Subscription must use pointer and integer operands, got: " +
                                    "'${typedExpression.resolvedType().toDisplayString()}' and " +
                                    "'${typedIndex.resolvedType().toDisplayString()}'",
                                typedExpression.location,
                            ),
                        )
                    }
                }
                expression.copy(
                    expression = typedExpression,
                    index = typedIndex,
                    type = type,
                )
            }
        }
    }

    private fun Type.Data.isInteger() = this is Type.Arithmetic && this !is Type.Double

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
        is AST.Expression.Subscript -> true
        else -> false
    }

    private fun getCommonType(type1: Type.Data, type2: Type.Data): Type.Data = when {
        type1 == type2 -> type1
        type1 == Type.Double || type2 == Type.Double -> Type.Double
        type1.byteSize() == type2.byteSize() -> {
            if (type1.signed()) {
                type2
            } else {
                type1
            }
        }
        type1.byteSize() > type2.byteSize() -> type1
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
