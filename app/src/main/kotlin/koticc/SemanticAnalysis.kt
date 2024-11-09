package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensureNotNull

data class ValidASTProgram(
    val value: AST.Program,
    val variableCount: Int,
)

data class SemanticAnalysisError(
    val message: String,
    val location: Location,
) : CompilerError {
    override fun message(): String = "semantic analysis error, at ${location.toHumanReadableString()}: $message"
}

fun validate(program: AST.Program): Either<SemanticAnalysisError, ValidASTProgram> = SemanticAnalysisContext().validateProgram(program)

private class SemanticAnalysisContext {
    val variableMapping: MutableMap<String, DeclaredVariable> = mutableMapOf()

    fun validateProgram(program: AST.Program): Either<SemanticAnalysisError, ValidASTProgram> =
        either {
            val validProgram =
                program.copy(
                    functionDefinition = validateFunctionDefinition(program.functionDefinition).bind(),
                )
            ValidASTProgram(validProgram, variableMapping.size)
        }

    private fun validateFunctionDefinition(
        functionDefinition: AST.FunctionDefinition,
    ): Either<SemanticAnalysisError, AST.FunctionDefinition> =
        either {
            functionDefinition.copy(body = functionDefinition.body.map { validateBlockItem(it).bind() })
        }

    private fun validateBlockItem(blockItem: AST.BlockItem): Either<SemanticAnalysisError, AST.BlockItem> =
        either {
            when (blockItem) {
                is AST.BlockItem.Declaration ->
                    AST.BlockItem.Declaration(validateDeclaration(blockItem.declaration).bind())
                is AST.BlockItem.Statement ->
                    AST.BlockItem.Statement(validateStatement(blockItem.statement).bind())
            }
        }

    private fun validateDeclaration(declaration: AST.Declaration): Either<SemanticAnalysisError, AST.Declaration> =
        either {
            val newName = "${declaration.name}.${variableMapping.size}"
            variableMapping[declaration.name]?.let {
                raise(
                    SemanticAnalysisError(
                        "variable '${declaration.name}' already declared at" +
                            " ${it.location.toHumanReadableString()}",
                        declaration.location,
                    ),
                )
            }
            variableMapping[declaration.name] = DeclaredVariable(newName, declaration.location)
            declaration.copy(name = newName, initializer = declaration.initializer?.let { validateExpression(it).bind() })
        }

    private fun validateStatement(statement: AST.Statement): Either<SemanticAnalysisError, AST.Statement> =
        either {
            when (statement) {
                is AST.Statement.Return ->
                    AST.Statement.Return(
                        expression = validateExpression(statement.expression).bind(),
                        location = statement.location,
                    )
                is AST.Statement.Expression ->
                    AST.Statement.Expression(
                        expression = validateExpression(statement.expression).bind(),
                    )
                is AST.Statement.Null -> statement
                is AST.Statement.If -> {
                    AST.Statement.If(
                        condition = validateExpression(statement.condition).bind(),
                        thenStatement = validateStatement(statement.thenStatement).bind(),
                        elseStatement = statement.elseStatement?.let { validateStatement(it).bind() },
                    )
                }
            }
        }

    private fun validateExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> =
        either {
            when (expression) {
                is AST.Expression.IntLiteral -> expression
                is AST.Expression.Variable -> {
                    val newName = variableMapping[expression.name]
                    ensureNotNull(newName) {
                        SemanticAnalysisError("undeclared variable '${expression.name}'", expression.location)
                    }
                    AST.Expression.Variable(newName.newName, expression.location)
                }
                is AST.Expression.Unary -> {
                    AST.Expression.Unary(
                        operator = expression.operator,
                        operand = validateExpression(expression.operand).bind(),
                        location = expression.location,
                    )
                }
                is AST.Expression.Binary -> {
                    AST.Expression.Binary(
                        operator = expression.operator,
                        left = validateExpression(expression.left).bind(),
                        right = validateExpression(expression.right).bind(),
                    )
                }
                is AST.Expression.Assignment -> {
                    validateAssignment(expression).bind()
                }

                is AST.Expression.CompoundAssignment -> {
                    validateCompoundAssignment(expression).bind()
                }
                is AST.Expression.Postfix -> {
                    validatePostfix(expression).bind()
                }
                is AST.Expression.Conditional -> {
                    AST.Expression.Conditional(
                        condition = validateExpression(expression.condition).bind(),
                        thenExpression = validateExpression(expression.thenExpression).bind(),
                        elseExpression = validateExpression(expression.elseExpression).bind(),
                    )
                }
            }
        }

    private fun validateAssignment(assignment: AST.Expression.Assignment): Either<SemanticAnalysisError, AST.Expression.Assignment> =
        either {
            val left =
                when (val result = validateExpression(assignment.left).bind()) {
                    is AST.Expression.Variable -> result
                    else ->
                        raise(
                            SemanticAnalysisError(
                                "left side of assignment must be a left-value, " +
                                    "got ${assignment.left}",
                                assignment.location,
                            ),
                        )
                }
            val right = validateExpression(assignment.right).bind()

            AST.Expression.Assignment(left, right)
        }

    private fun validateCompoundAssignment(
        assignment: AST.Expression.CompoundAssignment,
    ): Either<SemanticAnalysisError, AST.Expression.CompoundAssignment> =
        either {
            val left =
                when (val result = validateExpression(assignment.left).bind()) {
                    is AST.Expression.Variable -> result
                    else ->
                        raise(
                            SemanticAnalysisError(
                                "left side of compound assignment must be a left-value, " +
                                    "got ${assignment.left}",
                                assignment.location,
                            ),
                        )
                }
            val right = validateExpression(assignment.right).bind()

            AST.Expression.CompoundAssignment(assignment.operator, left, right)
        }

    private fun validatePostfix(postfix: AST.Expression.Postfix): Either<SemanticAnalysisError, AST.Expression.Postfix> =
        either {
            val operand =
                when (val result = validateExpression(postfix.operand).bind()) {
                    is AST.Expression.Variable -> result
                    else ->
                        raise(
                            SemanticAnalysisError(
                                "operand of postfix operator must be a left-value, " +
                                    "got ${postfix.operand}",
                                postfix.location,
                            ),
                        )
                }
            AST.Expression.Postfix(postfix.operator, operand)
        }
}

private data class DeclaredVariable(val newName: String, val location: Location)
