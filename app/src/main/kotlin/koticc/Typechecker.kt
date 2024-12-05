package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure

internal class Typechecker(private val nameMapping: Map<String, String>) {
    private val types: MutableMap<String, IdentifierType> = mutableMapOf()

    private fun originalIdentifierName(name: String): String = nameMapping[name]
        ?: error("unknown identifier name: $name, something is wrong in identifier resolver/typechecker")

    fun typecheck(program: AST.Program): Either<SemanticAnalysisError, Map<String, Type>> = either {
        program.functionDeclarations.forEach { functionDeclaration ->
            typecheckFunctionDeclaration(functionDeclaration).bind()
        }
        types.mapValues { (_, type) -> type.value }
    }

    private fun typecheckFunctionDeclaration(
        functionDeclaration: AST.Declaration.Function,
    ): Either<SemanticAnalysisError, Unit> = either {
        val functionType = Type.Function(parameterCount = functionDeclaration.parameters.size)
        val existingType = types[functionDeclaration.name]
        when (existingType) {
            is FunctionType -> {
                val bothHasBodies = existingType.hasBody && functionDeclaration.body != null
                val conflictingParameterCount = existingType.value.parameterCount != functionType.parameterCount
                if (bothHasBodies || conflictingParameterCount) {
                    raise(
                        SemanticAnalysisError(
                            "conflicting declaration of '${originalIdentifierName(functionDeclaration.name)}' " +
                                "at ${existingType.location.toHumanReadableString()}",
                            functionDeclaration.location,
                        ),
                    )
                }
            }
            else -> {}
        }
        types[functionDeclaration.name] = FunctionType(
            value = functionType,
            location = functionDeclaration.location,
            hasBody = functionDeclaration.body != null || (existingType as? FunctionType)?.hasBody == true,
        )
        functionDeclaration.body?.let {
            functionDeclaration.parameters.forEach { parameter ->
                types[parameter.name] = VariableType(value = Type.Integer, location = functionDeclaration.location)
            }
            typecheckBlock(it).bind()
        }
    }

    private fun typecheckBlock(block: AST.Block): Either<SemanticAnalysisError, Unit> = either {
        block.blockItems.forEach { blockItem ->
            when (blockItem) {
                is AST.BlockItem.Declaration -> when (blockItem.declaration) {
                    is AST.Declaration.Function -> typecheckFunctionDeclaration(blockItem.declaration).bind()
                    is AST.Declaration.Variable -> typecheckVariableDeclaration(blockItem.declaration).bind()
                }
                is AST.BlockItem.Statement -> typecheckStatement(blockItem.statement).bind()
            }
        }
    }

    private fun typecheckVariableDeclaration(
        variableDeclaration: AST.Declaration.Variable,
    ): Either<SemanticAnalysisError, Unit> = either {
        val variableType = Type.Integer
        types[variableDeclaration.name] = VariableType(value = variableType, location = variableDeclaration.location)
        variableDeclaration.initializer?.let { typecheckExpression(it).bind() }
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
                    is AST.ForInitializer.Declaration -> typecheckVariableDeclaration(statement.initializer.declaration).bind()
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
                val type = types[expression.name]
                ensure(type is FunctionType) {
                    SemanticAnalysisError("'${originalIdentifierName(expression.name)}' is not a function", expression.location)
                }
                ensure(type.value.parameterCount == expression.arguments.size) {
                    SemanticAnalysisError("function '${originalIdentifierName(expression.name)}' expects ${type.value.parameterCount} arguments, but ${expression.arguments.size} were provided", expression.location)
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
                val type = types[expression.name]
                ensure(type is VariableType) {
                    SemanticAnalysisError("'${originalIdentifierName(expression.name)}' is not a variable", expression.location)
                }
            }
        }
    }

    private sealed interface IdentifierType {
        val value: Type
        val location: Location
    }

    private data class VariableType(
        override val value: Type,
        override val location: Location,
    ) : IdentifierType

    private data class FunctionType(
        override val value: Type.Function,
        override val location: Location,
        val hasBody: Boolean,
    ) : IdentifierType
}
