package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import koticc.ast.AST
import koticc.ast.Type
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
        val (previouslyDefined, previouslyGlobal) = when (existingSymbol?.value) {
            is Symbol.Function -> {
                val bothDefined = existingSymbol.value.defined && functionDeclaration.body != null
                val conflictingTypes = existingSymbol.value.type != functionDeclaration.type
                if (bothDefined || conflictingTypes) {
                    raise(
                        SemanticAnalysisError(
                            "conflicting declaration of '${originalIdentifierName(functionDeclaration.name)}' " +
                                "at ${existingSymbol.location.toDisplayString()}",
                            functionDeclaration.location,
                        ),
                    )
                }
                if (existingSymbol.value.global && functionDeclaration.storageClass == AST.StorageClass.Static) {
                    raise(
                        SemanticAnalysisError(
                            "static function '${originalIdentifierName(functionDeclaration.name)}' declaration " +
                                "follows non-static at ${existingSymbol.location.toDisplayString()}",
                            functionDeclaration.location,
                        ),
                    )
                }
                existingSymbol.value.defined to existingSymbol.value.global
            }
            is Symbol.Variable -> raise(
                SemanticAnalysisError(
                    "variable '${originalIdentifierName(functionDeclaration.name)}' redeclared as a function",
                    functionDeclaration.location,
                ),
            )
            null -> {
                false to null
            }
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
                functionDeclaration.parameters.forEach { parameter ->
                    symbolTable[parameter.name] = SymbolWithLocation(
                        value = Symbol.Variable(type = Type.Int, attributes = VariableAttributes.Local),
                        location = functionDeclaration.location,
                    )
                }
                typecheckBlock(it).bind()
            },
        )
    }

    private fun typecheckFileScopeVariableDeclaration(
        declaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> = either {
        var initialValue = when (declaration.initializer) {
            is AST.Expression.Constant -> {
                when (declaration.initializer.value) {
                    is AST.IntConstant -> InitialValue.Constant(declaration.initializer.value.value)
                    is AST.LongConstant -> TODO()
                }
            }
            null -> {
                when (declaration.storageClass) {
                    AST.StorageClass.Extern -> InitialValue.NoInitializer
                    AST.StorageClass.Static -> InitialValue.Tentative
                    null -> InitialValue.Tentative
                }
            }
            else -> raise(
                SemanticAnalysisError(
                    "non-constant initializer for file-scope variable '${originalIdentifierName(declaration.name)}'",
                    declaration.location,
                ),
            )
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

            val existingInitialValue = existingVariable.attributes.initialValue
            when (existingInitialValue) {
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
                type = Type.Int,
                attributes = VariableAttributes.Static(
                    initialValue = initialValue,
                    global = global,
                ),
            ),
            location = declaration.location,
        )
        declaration.copy(
            initializer = declaration.initializer?.let { typecheckExpression(it).bind() },
        )
    }

    private fun typecheckBlock(block: AST.Block): Either<SemanticAnalysisError, AST.Block> = either {
        block.copy(
            blockItems = block.blockItems.map { blockItem ->
                when (blockItem) {
                    is AST.BlockItem.Declaration -> blockItem.copy(
                        declaration = typecheckDeclaration(blockItem.declaration).bind(),
                    )
                    is AST.BlockItem.Statement -> blockItem.copy(
                        statement = typecheckStatement(blockItem.statement).bind(),
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
                when (existingSymbol?.value) {
                    is Symbol.Function -> raise(
                        SemanticAnalysisError(
                            "function '${originalIdentifierName(variableDeclaration.name)}' redeclared as a variable",
                            variableDeclaration.location,
                        ),
                    )
                    is Symbol.Variable -> {}
                    null -> {
                        symbolTable[variableDeclaration.name] = SymbolWithLocation(
                            value = Symbol.Variable(
                                type = Type.Int,
                                attributes = VariableAttributes.Static(
                                    initialValue = InitialValue.NoInitializer,
                                    global = true,
                                ),
                            ),
                            location = variableDeclaration.location,
                        )
                    }
                }
            }
            AST.StorageClass.Static -> {
                val initialValue = when (variableDeclaration.initializer) {
                    is AST.Expression.Constant -> {
                        when (variableDeclaration.initializer.value) {
                            is AST.IntConstant -> InitialValue.Constant(variableDeclaration.initializer.value.value)
                            is AST.LongConstant -> TODO()
                        }
                    }
                    null -> InitialValue.Constant(0)
                    else -> raise(
                        SemanticAnalysisError(
                            "non-constant initializer for local static variable '${originalIdentifierName(variableDeclaration.name)}'",
                            variableDeclaration.location,
                        ),
                    )
                }
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(
                        type = Type.Int,
                        attributes = VariableAttributes.Static(
                            initialValue = initialValue,
                            global = false,
                        ),
                    ),
                    location = variableDeclaration.location,
                )
            }
            null -> {
                val variableType = Type.Int
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(type = variableType, attributes = VariableAttributes.Local),
                    location = variableDeclaration.location,
                )
                variableDeclaration.initializer?.let { typecheckExpression(it).bind() }
            }
        }
        variableDeclaration.copy(
            initializer = variableDeclaration.initializer?.let { typecheckExpression(it).bind() },
        )
    }

    private fun typecheckStatement(statement: AST.Statement): Either<SemanticAnalysisError, AST.Statement> = either {
        when (statement) {
            is AST.Statement.Break -> statement
            is AST.Statement.BreakLoop -> statement
            is AST.Statement.BreakSwitch -> statement
            is AST.Statement.Case -> statement.copy(
                expression = typecheckExpression(statement.expression).bind(),
                body = typecheckStatement(statement.body).bind(),
            )
            is AST.Statement.Compound -> statement.copy(block = typecheckBlock(statement.block).bind())
            is AST.Statement.Continue -> statement
            is AST.Statement.Default -> statement.copy(
                body = typecheckStatement(statement.body).bind(),
            )
            is AST.Statement.DoWhile -> {
                statement.copy(
                    body = typecheckStatement(statement.body).bind(),
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
                    body = typecheckStatement(statement.body).bind(),
                )
            }
            is AST.Statement.Goto -> statement
            is AST.Statement.If -> {
                statement.copy(
                    condition = typecheckExpression(statement.condition).bind(),
                    thenStatement = typecheckStatement(statement.thenStatement).bind(),
                    elseStatement = statement.elseStatement?.let { typecheckStatement(it).bind() },
                )
            }
            is AST.Statement.Labeled -> statement.copy(
                statement = typecheckStatement(statement.statement).bind(),
            )
            is AST.Statement.Null -> statement
            is AST.Statement.Return -> {
                statement.copy(
                    expression = typecheckExpression(statement.expression).bind(),
                )
            }
            is AST.Statement.Switch -> {
                statement.copy(
                    expression = typecheckExpression(statement.expression).bind(),
                    body = typecheckStatement(statement.body).bind(),
                )
            }
            is AST.Statement.While -> {
                statement.copy(
                    condition = typecheckExpression(statement.condition).bind(),
                    body = typecheckStatement(statement.body).bind(),
                )
            }
        }
    }

    private fun typecheckExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> = either {
        when (expression) {
            is AST.Expression.Assignment -> {
                expression.copy(
                    left = typecheckExpression(expression.left).bind(),
                    right = typecheckExpression(expression.right).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.Binary -> {
                expression.copy(
                    left = typecheckExpression(expression.left).bind(),
                    right = typecheckExpression(expression.right).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.CompoundAssignment -> {
                expression.copy(
                    left = typecheckExpression(expression.left).bind(),
                    right = typecheckExpression(expression.right).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.Conditional -> {
                expression.copy(
                    condition = typecheckExpression(expression.condition).bind(),
                    thenExpression = typecheckExpression(expression.thenExpression).bind(),
                    elseExpression = typecheckExpression(expression.elseExpression).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.FunctionCall -> {
                val functionSymbol = symbolTable[expression.name]
                val functionType = when (functionSymbol?.value) {
                    is Symbol.Function -> functionSymbol.value
                    else -> raise(
                        SemanticAnalysisError(
                            "'${originalIdentifierName(expression.name)}' is not a function",
                            expression.location,
                        ),
                    )
                }
                ensure(functionType.type.parameters.size == expression.arguments.size) {
                    SemanticAnalysisError("function '${originalIdentifierName(expression.name)}' expects ${functionSymbol.value.type.parameters.size} arguments, but ${expression.arguments.size} were provided", expression.location)
                }
                expression.copy(
                    arguments = expression.arguments.map { typecheckExpression(it).bind() },
                ).ofType(Type.Int)
            }
            is AST.Expression.Constant -> {
                expression.ofType(Type.Int)
            }
            is AST.Expression.Postfix -> {
                expression.copy(
                    operand = typecheckExpression(expression.operand).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.Unary -> {
                expression.copy(
                    operand = typecheckExpression(expression.operand).bind(),
                ).ofType(Type.Int)
            }
            is AST.Expression.Variable -> {
                val type = symbolTable[expression.name]
                ensure(type?.value is Symbol.Variable) {
                    SemanticAnalysisError("'${originalIdentifierName(expression.name)}' is not a variable", expression.location)
                }
                expression.ofType(Type.Int)
            }
        }
    }

    private data class SymbolWithLocation(
        val value: Symbol,
        val location: Location,
    )
}
