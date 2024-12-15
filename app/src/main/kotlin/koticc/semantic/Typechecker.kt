package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import koticc.ast.AST
import koticc.ast.Type
import koticc.token.Location

internal class Typechecker(private val nameMapping: Map<String, String>) {
    private val symbolTable: MutableMap<String, SymbolWithLocation> = mutableMapOf()

    private fun originalIdentifierName(name: String): String = nameMapping[name]
        ?: name

    fun typecheck(program: AST.Program): Either<SemanticAnalysisError, SymbolTable> = either {
        program.declarations.forEach { declaration ->
            when (declaration) {
                is AST.Declaration.Function -> typecheckFunctionDeclaration(declaration).bind()
                is AST.Declaration.Variable -> typecheckFileScopeVariableDeclaration(declaration).bind()
            }
        }
        symbolTable.mapValues { (_, symbolWithLocation) -> symbolWithLocation.value }
    }

    private fun typecheckDeclaration(
        declaration: AST.Declaration,
    ): Either<SemanticAnalysisError, Unit> = either {
        when (declaration) {
            is AST.Declaration.Function -> typecheckFunctionDeclaration(declaration).bind()
            is AST.Declaration.Variable -> typecheckVariableDeclaration(declaration).bind()
        }
    }

    private fun typecheckFunctionDeclaration(
        functionDeclaration: AST.Declaration.Function,
    ): Either<SemanticAnalysisError, Unit> = either {
        val functionType = Type.Function(parameterCount = functionDeclaration.parameters.size)
        val global = functionDeclaration.storageClass != AST.StorageClass.Static
        val existingSymbol = symbolTable[functionDeclaration.name]
        val (previouslyDefined, previouslyGlobal) = when (existingSymbol?.value) {
            is Symbol.Function -> {
                val bothDefined = existingSymbol.value.defined && functionDeclaration.body != null
                val conflictingTypes = existingSymbol.value.type != functionType
                if (bothDefined || conflictingTypes) {
                    raise(
                        SemanticAnalysisError(
                            "conflicting declaration of '${originalIdentifierName(functionDeclaration.name)}' " +
                                "at ${existingSymbol.location.toHumanReadableString()}",
                            functionDeclaration.location,
                        ),
                    )
                }
                if (existingSymbol.value.global && functionDeclaration.storageClass == AST.StorageClass.Static) {
                    raise(
                        SemanticAnalysisError(
                            "static function '${originalIdentifierName(functionDeclaration.name)}' declaration " +
                                "follows non-static at ${existingSymbol.location.toHumanReadableString()}",
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
                type = functionType,
                defined = functionDeclaration.body != null || previouslyDefined,
                global = previouslyGlobal ?: global,
            ),
            location = functionDeclaration.location,
        )
        functionDeclaration.body?.let {
            functionDeclaration.parameters.forEach { parameter ->
                symbolTable[parameter.name] = SymbolWithLocation(
                    value = Symbol.Variable(type = Type.Integer, attributes = VariableAttributes.Local),
                    location = functionDeclaration.location,
                )
            }
            typecheckBlock(it).bind()
        }
    }

    private fun typecheckFileScopeVariableDeclaration(
        declaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, Unit> = either {
        var initialValue = when (declaration.initializer) {
            is AST.Expression.IntLiteral -> InitialValue.Constant(declaration.initializer.value)
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
                            "follows non-static at ${existingSymbol.location.toHumanReadableString()}",
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
                type = Type.Integer,
                attributes = VariableAttributes.Static(
                    initialValue = initialValue,
                    global = global,
                ),
            ),
            location = declaration.location,
        )
    }

    private fun typecheckBlock(block: AST.Block): Either<SemanticAnalysisError, Unit> = either {
        block.blockItems.forEach { blockItem ->
            when (blockItem) {
                is AST.BlockItem.Declaration -> typecheckDeclaration(blockItem.declaration).bind()
                is AST.BlockItem.Statement -> typecheckStatement(blockItem.statement).bind()
            }
        }
    }

    private fun typecheckVariableDeclaration(
        variableDeclaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, Unit> = either {
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
                                type = Type.Integer,
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
                    is AST.Expression.IntLiteral -> InitialValue.Constant(variableDeclaration.initializer.value)
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
                        type = Type.Integer,
                        attributes = VariableAttributes.Static(
                            initialValue = initialValue,
                            global = false,
                        ),
                    ),
                    location = variableDeclaration.location,
                )
            }
            null -> {
                val variableType = Type.Integer
                symbolTable[variableDeclaration.name] = SymbolWithLocation(
                    value = Symbol.Variable(type = variableType, attributes = VariableAttributes.Local),
                    location = variableDeclaration.location,
                )
                variableDeclaration.initializer?.let { typecheckExpression(it).bind() }
            }
        }
    }

    private fun typecheckStatement(statement: AST.Statement): Either<SemanticAnalysisError, Unit> = either {
        when (statement) {
            is AST.Statement.Break -> {}
            is AST.Statement.BreakLoop -> {}
            is AST.Statement.BreakSwitch -> {}
            is AST.Statement.Case -> typecheckStatement(statement.body).bind()
            is AST.Statement.Compound -> typecheckBlock(statement.block).bind()
            is AST.Statement.Continue -> {}
            is AST.Statement.Default -> typecheckStatement(statement.body).bind()
            is AST.Statement.DoWhile -> {
                typecheckStatement(statement.body).bind()
                typecheckExpression(statement.condition).bind()
            }
            is AST.Statement.Expression -> typecheckExpression(statement.expression).bind()
            is AST.Statement.For -> {
                when (statement.initializer) {
                    is AST.ForInitializer.Declaration -> {
                        ensure(statement.initializer.declaration.storageClass == null) {
                            SemanticAnalysisError(
                                "can't use storage class specifier in for loop initializer",
                                statement.initializer.declaration.location,
                            )
                        }
                        typecheckVariableDeclaration(statement.initializer.declaration).bind()
                    }
                    is AST.ForInitializer.Expression -> typecheckExpression(statement.initializer.expression).bind()
                    null -> {}
                }
                statement.condition?.let { typecheckExpression(it).bind() }
                statement.post?.let { typecheckExpression(it).bind() }
                typecheckStatement(statement.body).bind()
            }
            is AST.Statement.Goto -> {}
            is AST.Statement.If -> {
                typecheckExpression(statement.condition).bind()
                typecheckStatement(statement.thenStatement).bind()
                statement.elseStatement?.let { typecheckStatement(it).bind() }
            }
            is AST.Statement.Labeled -> typecheckStatement(statement.statement).bind()
            is AST.Statement.Null -> {}
            is AST.Statement.Return -> statement.expression.let { typecheckExpression(it).bind() }
            is AST.Statement.Switch -> {
                typecheckExpression(statement.expression).bind()
                typecheckStatement(statement.body).bind()
            }
            is AST.Statement.While -> {
                typecheckExpression(statement.condition).bind()
                typecheckStatement(statement.body).bind()
            }
        }
    }

    private fun typecheckExpression(expression: AST.Expression): Either<SemanticAnalysisError, Unit> = either {
        when (expression) {
            is AST.Expression.Assignment -> {
                typecheckExpression(expression.left).bind()
                typecheckExpression(expression.right).bind()
            }
            is AST.Expression.Binary -> {
                typecheckExpression(expression.left).bind()
                typecheckExpression(expression.right).bind()
            }
            is AST.Expression.CompoundAssignment -> {
                typecheckExpression(expression.left).bind()
                typecheckExpression(expression.right).bind()
            }
            is AST.Expression.Conditional -> {
                typecheckExpression(expression.condition).bind()
                typecheckExpression(expression.thenExpression).bind()
                typecheckExpression(expression.elseExpression).bind()
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
                ensure(functionType.type.parameterCount == expression.arguments.size) {
                    SemanticAnalysisError("function '${originalIdentifierName(expression.name)}' expects ${functionSymbol.value.type.parameterCount} arguments, but ${expression.arguments.size} were provided", expression.location)
                }
            }
            is AST.Expression.IntLiteral -> {}
            is AST.Expression.Postfix -> {
                typecheckExpression(expression.operand).bind()
            }
            is AST.Expression.Unary -> {
                typecheckExpression(expression.operand).bind()
            }
            is AST.Expression.Variable -> {
                val type = symbolTable[expression.name]
                ensure(type?.value is Symbol.Variable) {
                    SemanticAnalysisError("'${originalIdentifierName(expression.name)}' is not a variable", expression.location)
                }
            }
        }
    }

    private data class SymbolWithLocation(
        val value: Symbol,
        val location: Location,
    )
}
