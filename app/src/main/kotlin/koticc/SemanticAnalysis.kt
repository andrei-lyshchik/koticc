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
    private var variableCount = 0

    fun resolveProgram(program: AST.Program): Either<SemanticAnalysisError, VariableResolverResult> =
        either {
            val validProgram =
                program.copy(
                    functionDefinition = resolveFunctionDefinition(program.functionDefinition).bind(),
                )
            VariableResolverResult(validProgram, variableCount)
        }

    private fun resolveFunctionDefinition(
        functionDefinition: AST.FunctionDefinition,
    ): Either<SemanticAnalysisError, AST.FunctionDefinition> =
        either {
            functionDefinition.copy(body = resolveBlock(functionDefinition.body, variableMapping = mutableMapOf()).bind())
        }

    private fun resolveBlock(
        block: AST.Block,
        variableMapping: MutableMap<String, DeclaredVariable>,
    ): Either<SemanticAnalysisError, AST.Block> {
        val variableMappingCopy = variableMapping.copyForNestedBlock()
        return either {
            block.copy(blockItems = block.blockItems.map { resolveBlockItem(it, variableMappingCopy).bind() })
        }
    }

    private fun resolveBlockItem(blockItem: AST.BlockItem, variableMapping: MutableMap<String, DeclaredVariable>): Either<SemanticAnalysisError, AST.BlockItem> =
        either {
            when (blockItem) {
                is AST.BlockItem.Declaration ->
                    AST.BlockItem.Declaration(resolveDeclaration(blockItem.declaration, variableMapping).bind())
                is AST.BlockItem.Statement ->
                    AST.BlockItem.Statement(resolveStatement(blockItem.statement, variableMapping).bind())
            }
        }

    private fun resolveDeclaration(declaration: AST.Declaration, variableMapping: MutableMap<String, DeclaredVariable>): Either<SemanticAnalysisError, AST.Declaration> =
        either {
            val newName = "${declaration.name}.${variableCount++}"
            variableMapping[declaration.name]?.takeIf { it.declaredInThisScope }?.let {
                raise(
                    SemanticAnalysisError(
                        "variable '${declaration.name}' already declared at" +
                            " ${it.location.toHumanReadableString()}",
                        declaration.location,
                    ),
                )
            }
            variableMapping[declaration.name] = DeclaredVariable(newName, declaration.location, declaredInThisScope = true)
            declaration.copy(name = newName, initializer = declaration.initializer?.let { resolveExpression(it, variableMapping).bind() })
        }

    private fun resolveStatement(statement: AST.Statement, variableMapping: MutableMap<String, DeclaredVariable>): Either<SemanticAnalysisError, AST.Statement> =
        either {
            when (statement) {
                is AST.Statement.Return ->
                    AST.Statement.Return(
                        expression = resolveExpression(statement.expression, variableMapping).bind(),
                        location = statement.location,
                    )
                is AST.Statement.Expression ->
                    AST.Statement.Expression(
                        expression = resolveExpression(statement.expression, variableMapping).bind(),
                    )
                is AST.Statement.Null -> statement
                is AST.Statement.If -> {
                    AST.Statement.If(
                        condition = resolveExpression(statement.condition, variableMapping).bind(),
                        thenStatement = resolveStatement(statement.thenStatement, variableMapping).bind(),
                        elseStatement = statement.elseStatement?.let { resolveStatement(it, variableMapping).bind() },
                    )
                }
                is AST.Statement.Labeled -> {
                    AST.Statement.Labeled(
                        label = statement.label,
                        statement = resolveStatement(statement.statement, variableMapping).bind(),
                        location = statement.location,
                    )
                }
                is AST.Statement.Goto -> statement
                is AST.Statement.Compound -> AST.Statement.Compound(resolveBlock(statement.block, variableMapping).bind())
            }
        }

    private fun resolveExpression(expression: AST.Expression, variableMapping: MutableMap<String, DeclaredVariable>): Either<SemanticAnalysisError, AST.Expression> =
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
                        operand = resolveExpression(expression.operand, variableMapping).bind(),
                        location = expression.location,
                    )
                }
                is AST.Expression.Binary -> {
                    AST.Expression.Binary(
                        operator = expression.operator,
                        left = resolveExpression(expression.left, variableMapping).bind(),
                        right = resolveExpression(expression.right, variableMapping).bind(),
                    )
                }
                is AST.Expression.Assignment -> {
                    resolveAssignment(expression, variableMapping).bind()
                }

                is AST.Expression.CompoundAssignment -> {
                    resolveCompoundAssignment(expression, variableMapping).bind()
                }
                is AST.Expression.Postfix -> {
                    resolvePostfix(expression, variableMapping).bind()
                }
                is AST.Expression.Conditional -> {
                    AST.Expression.Conditional(
                        condition = resolveExpression(expression.condition, variableMapping).bind(),
                        thenExpression = resolveExpression(expression.thenExpression, variableMapping).bind(),
                        elseExpression = resolveExpression(expression.elseExpression, variableMapping).bind(),
                    )
                }
            }
        }

    private fun resolveAssignment(
        assignment: AST.Expression.Assignment,
        variableMapping: MutableMap<String, DeclaredVariable>,
    ): Either<SemanticAnalysisError, AST.Expression.Assignment> =
        either {
            val left =
                when (val result = resolveExpression(assignment.left, variableMapping).bind()) {
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
            val right = resolveExpression(assignment.right, variableMapping).bind()

            AST.Expression.Assignment(left, right)
        }

    private fun resolveCompoundAssignment(
        assignment: AST.Expression.CompoundAssignment,
        variableMapping: MutableMap<String, DeclaredVariable>,
    ): Either<SemanticAnalysisError, AST.Expression.CompoundAssignment> =
        either {
            val left =
                when (val result = resolveExpression(assignment.left, variableMapping = variableMapping).bind()) {
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
            val right = resolveExpression(assignment.right, variableMapping).bind()

            AST.Expression.CompoundAssignment(assignment.operator, left, right)
        }

    private fun resolvePostfix(postfix: AST.Expression.Postfix, variableMapping: MutableMap<String, DeclaredVariable>): Either<SemanticAnalysisError, AST.Expression.Postfix> =
        either {
            val operand =
                when (val result = resolveExpression(postfix.operand, variableMapping = variableMapping).bind()) {
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

    private data class DeclaredVariable(val newName: String, val location: Location, val declaredInThisScope: Boolean)

    private fun MutableMap<String, DeclaredVariable>.copyForNestedBlock() =
        mapValues { (key, value) -> value.copy(declaredInThisScope = false) }.toMutableMap()
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
    program.functionDefinition.body.blockItems.filterIsInstance<AST.BlockItem.Statement>()
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
        is AST.Statement.Compound ->
            statement.block.blockItems
                .filterIsInstance<AST.BlockItem.Statement>()
                .map(AST.BlockItem.Statement::statement)
                .flatMap(::findAllLabeledStatements)
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
    program.functionDefinition.body.blockItems.filterIsInstance<AST.BlockItem.Statement>()
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
        is AST.Statement.Compound ->
            statement.block.blockItems
                .filterIsInstance<AST.BlockItem.Statement>()
                .map(AST.BlockItem.Statement::statement)
                .flatMap(::findAllGotos)
    }
