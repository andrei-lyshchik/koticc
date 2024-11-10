package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
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

fun semanticAnalysis(program: AST.Program): Either<SemanticAnalysisError, ValidASTProgram> =
    either {
        val variableResolverResult = VariableResolver().resolveProgram(program).bind()
        validateLabels(variableResolverResult.program).bind()
        ValidASTProgram(variableResolverResult.program, variableResolverResult.variableCount)
    }

private data class VariableResolverResult(
    val program: AST.Program,
    val variableCount: Int,
)

private class VariableResolver {
    private val variableMapping: MutableMap<String, DeclaredVariable> = mutableMapOf()

    fun resolveProgram(program: AST.Program): Either<SemanticAnalysisError, VariableResolverResult> =
        either {
            val validProgram =
                program.copy(
                    functionDefinition = resolveFunctionDefinition(program.functionDefinition).bind(),
                )
            VariableResolverResult(validProgram, variableMapping.size)
        }

    private fun resolveFunctionDefinition(
        functionDefinition: AST.FunctionDefinition,
    ): Either<SemanticAnalysisError, AST.FunctionDefinition> =
        either {
            functionDefinition.copy(body = functionDefinition.body.map { resolveBlockItem(it).bind() })
        }

    private fun resolveBlockItem(blockItem: AST.BlockItem): Either<SemanticAnalysisError, AST.BlockItem> =
        either {
            when (blockItem) {
                is AST.BlockItem.Declaration ->
                    AST.BlockItem.Declaration(resolveDeclaration(blockItem.declaration).bind())
                is AST.BlockItem.Statement ->
                    AST.BlockItem.Statement(validateStatement(blockItem.statement).bind())
            }
        }

    private fun resolveDeclaration(declaration: AST.Declaration): Either<SemanticAnalysisError, AST.Declaration> =
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
            declaration.copy(name = newName, initializer = declaration.initializer?.let { resolveExpression(it).bind() })
        }

    private fun validateStatement(statement: AST.Statement): Either<SemanticAnalysisError, AST.Statement> =
        either {
            when (statement) {
                is AST.Statement.Return ->
                    AST.Statement.Return(
                        expression = resolveExpression(statement.expression).bind(),
                        location = statement.location,
                    )
                is AST.Statement.Expression ->
                    AST.Statement.Expression(
                        expression = resolveExpression(statement.expression).bind(),
                    )
                is AST.Statement.Null -> statement
                is AST.Statement.If -> {
                    AST.Statement.If(
                        condition = resolveExpression(statement.condition).bind(),
                        thenStatement = validateStatement(statement.thenStatement).bind(),
                        elseStatement = statement.elseStatement?.let { validateStatement(it).bind() },
                    )
                }
                is AST.Statement.Labeled -> {
                    AST.Statement.Labeled(
                        label = statement.label,
                        statement = validateStatement(statement.statement).bind(),
                        location = statement.location,
                    )
                }
                is AST.Statement.Goto -> statement
            }
        }

    private fun resolveExpression(expression: AST.Expression): Either<SemanticAnalysisError, AST.Expression> =
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
                        operand = resolveExpression(expression.operand).bind(),
                        location = expression.location,
                    )
                }
                is AST.Expression.Binary -> {
                    AST.Expression.Binary(
                        operator = expression.operator,
                        left = resolveExpression(expression.left).bind(),
                        right = resolveExpression(expression.right).bind(),
                    )
                }
                is AST.Expression.Assignment -> {
                    resolveAssignment(expression).bind()
                }

                is AST.Expression.CompoundAssignment -> {
                    resolveCompoundAssignment(expression).bind()
                }
                is AST.Expression.Postfix -> {
                    resolvePostfix(expression).bind()
                }
                is AST.Expression.Conditional -> {
                    AST.Expression.Conditional(
                        condition = resolveExpression(expression.condition).bind(),
                        thenExpression = resolveExpression(expression.thenExpression).bind(),
                        elseExpression = resolveExpression(expression.elseExpression).bind(),
                    )
                }
            }
        }

    private fun resolveAssignment(assignment: AST.Expression.Assignment): Either<SemanticAnalysisError, AST.Expression.Assignment> =
        either {
            val left =
                when (val result = resolveExpression(assignment.left).bind()) {
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
            val right = resolveExpression(assignment.right).bind()

            AST.Expression.Assignment(left, right)
        }

    private fun resolveCompoundAssignment(
        assignment: AST.Expression.CompoundAssignment,
    ): Either<SemanticAnalysisError, AST.Expression.CompoundAssignment> =
        either {
            val left =
                when (val result = resolveExpression(assignment.left).bind()) {
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
            val right = resolveExpression(assignment.right).bind()

            AST.Expression.CompoundAssignment(assignment.operator, left, right)
        }

    private fun resolvePostfix(postfix: AST.Expression.Postfix): Either<SemanticAnalysisError, AST.Expression.Postfix> =
        either {
            val operand =
                when (val result = resolveExpression(postfix.operand).bind()) {
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

    private data class DeclaredVariable(val newName: String, val location: Location)
}

private fun validateLabels(program: AST.Program): Either<SemanticAnalysisError, AST.Program> =
    either {
        val labelMapping = validateLabelsAreUnique(program).bind()
        validateGotos(program, labelMapping).bind()
        program
    }

private fun validateLabelsAreUnique(program: AST.Program) =
    either<SemanticAnalysisError, Map<LabelName, Location>> {
        val labelMapping = mutableMapOf<LabelName, Location>()
        findAllLabeledStatements(program)
            .forEach { labeledStatement ->
                val existingLocation = labelMapping[labeledStatement.label]
                ensure(existingLocation == null) {
                    SemanticAnalysisError(
                        "label '${labeledStatement.label.value}' already declared at" +
                            " ${labelMapping[labeledStatement.label]!!.toHumanReadableString()}",
                        labeledStatement.location,
                    )
                }
                labelMapping[labeledStatement.label] = labeledStatement.location
            }
        labelMapping
    }

private fun findAllLabeledStatements(program: AST.Program): List<AST.Statement.Labeled> =
    program.functionDefinition.body.filterIsInstance<AST.BlockItem.Statement>()
        .map(AST.BlockItem.Statement::statement)
        .flatMap(::findAllLabeledStatements)

private fun findAllLabeledStatements(statement: AST.Statement): List<AST.Statement.Labeled> =
    when (statement) {
        is AST.Statement.Expression -> emptyList()
        is AST.Statement.Goto -> emptyList()
        is AST.Statement.If ->
            findAllLabeledStatements(statement.thenStatement) +
                (statement.elseStatement?.let { findAllLabeledStatements(it) } ?: emptyList())
        is AST.Statement.Labeled -> listOf(statement) + findAllLabeledStatements(statement.statement)
        is AST.Statement.Null -> emptyList()
        is AST.Statement.Return -> emptyList()
    }

private fun validateGotos(
    program: AST.Program,
    labelMapping: Map<LabelName, Location>,
) = either {
    findAllGotos(program)
        .forEach { goto ->
            val existingLocation = labelMapping[goto.label]
            ensureNotNull(existingLocation) {
                SemanticAnalysisError("goto to undeclared label '${goto.label.value}'", goto.location)
            }
        }
}

private fun findAllGotos(program: AST.Program): List<AST.Statement.Goto> =
    program.functionDefinition.body.filterIsInstance<AST.BlockItem.Statement>()
        .map(AST.BlockItem.Statement::statement)
        .flatMap(::findAllGotos)

private fun findAllGotos(statement: AST.Statement): List<AST.Statement.Goto> =
    when (statement) {
        is AST.Statement.Expression -> emptyList()
        is AST.Statement.Goto -> listOf(statement)
        is AST.Statement.If -> findAllGotos(statement.thenStatement) + (statement.elseStatement?.let(::findAllGotos) ?: emptyList())
        is AST.Statement.Labeled -> findAllGotos(statement.statement)
        is AST.Statement.Null -> emptyList()
        is AST.Statement.Return -> emptyList()
    }
