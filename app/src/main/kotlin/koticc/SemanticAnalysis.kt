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
        validateGotoLabels(variableResolverResult.program).bind()
        val programWithResolvedLoopsAndSwitches = LoopAndSwitchResolver().resolveLoopsAndSwitches(variableResolverResult.program).bind()
        ValidASTProgram(
            value = programWithResolvedLoopsAndSwitches,
            variableCount = variableResolverResult.variableCount,
        )
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
                    functionDeclarations = program.functionDeclarations.map {
                        resolveFunctionDefinition(it).bind()
                    },
                )
            VariableResolverResult(validProgram, variableCount)
        }

    private fun resolveFunctionDefinition(
        functionDeclaration: AST.Declaration.Function,
    ): Either<SemanticAnalysisError, AST.Declaration.Function> =
        either {
            functionDeclaration.copy(body = functionDeclaration.body?.let { resolveBlock(it, variableMapping = mutableMapOf()).bind() })
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
                is AST.BlockItem.Declaration -> when (blockItem.declaration) {
                    is AST.Declaration.Variable ->
                        AST.BlockItem.Declaration(resolveVariableDeclaration(blockItem.declaration, variableMapping).bind())
                    is AST.Declaration.Function -> blockItem
                }
                is AST.BlockItem.Statement ->
                    AST.BlockItem.Statement(resolveStatement(blockItem.statement, variableMapping).bind())
            }
        }

    private fun resolveVariableDeclaration(
        declaration: AST.Declaration.Variable,
        variableMapping: MutableMap<String, DeclaredVariable>,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> =
        either {
            variableMapping[declaration.name]?.takeIf { it.declaredInThisScope }?.let {
                raise(
                    SemanticAnalysisError(
                        "variable '${declaration.name}' already declared at" +
                            " ${it.location.toHumanReadableString()}",
                        declaration.location,
                    ),
                )
            }
            val newName = "${declaration.name}.${variableCount++}"
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
                is AST.Statement.DoWhile -> {
                    AST.Statement.DoWhile(
                        body = resolveStatement(statement.body, variableMapping).bind(),
                        condition = resolveExpression(statement.condition, variableMapping).bind(),
                        loopId = statement.loopId,
                        location = statement.location,
                    )
                }
                is AST.Statement.While -> AST.Statement.While(
                    condition = resolveExpression(statement.condition, variableMapping).bind(),
                    body = resolveStatement(statement.body, variableMapping).bind(),
                    loopId = statement.loopId,
                    location = statement.location,
                )
                is AST.Statement.For -> {
                    val headerVariableMapping = variableMapping.copyForNestedBlock()
                    val initializer = when (statement.initializer) {
                        is AST.ForInitializer.Declaration ->
                            AST.ForInitializer.Declaration(
                                resolveVariableDeclaration(statement.initializer.declaration, headerVariableMapping).bind(),
                            )

                        is AST.ForInitializer.Expression ->
                            AST.ForInitializer.Expression(
                                resolveExpression(statement.initializer.expression, headerVariableMapping).bind(),
                            )

                        null -> null
                    }
                    val condition = statement.condition?.let { resolveExpression(it, headerVariableMapping).bind() }
                    val post = statement.post?.let { resolveExpression(it, headerVariableMapping).bind() }
                    val body = resolveStatement(statement.body, headerVariableMapping).bind()
                    AST.Statement.For(
                        initializer = initializer,
                        condition = condition,
                        post = post,
                        body = body,
                        loopId = statement.loopId,
                        location = statement.location,
                    )
                }

                is AST.Statement.Break -> statement
                is AST.Statement.Continue -> statement
                is AST.Statement.Case -> statement.copy(
                    expression = resolveExpression(statement.expression, variableMapping).bind(),
                    body = resolveStatement(statement.body, variableMapping).bind(),
                )
                is AST.Statement.Default -> statement.copy(
                    body = resolveStatement(statement.body, variableMapping).bind(),
                )
                is AST.Statement.Switch -> statement.copy(
                    expression = resolveExpression(statement.expression, variableMapping).bind(),
                    body = resolveStatement(statement.body, variableMapping).bind(),
                    caseExpressions = statement.caseExpressions?.mapKeys { (key, _) -> key },
                )
                is AST.Statement.BreakLoop -> statement
                is AST.Statement.BreakSwitch -> statement
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
                is AST.Expression.FunctionCall -> {
                    expression.copy(
                        arguments = expression.arguments.map { resolveExpression(it, variableMapping).bind() },
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
        mapValues { (_, value) -> value.copy(declaredInThisScope = false) }.toMutableMap()
}

private fun validateGotoLabels(program: AST.Program): Either<SemanticAnalysisError, AST.Program> =
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
    program.functionDeclarations.flatMap { it.body?.let(::findAllLabeledStatements) ?: emptyList() }

private fun findAllLabeledStatements(block: AST.Block): List<AST.Statement.Labeled> =
    block.blockItems.filterIsInstance<AST.BlockItem.Statement>()
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
            findAllLabeledStatements(statement.block)
        is AST.Statement.DoWhile -> findAllLabeledStatements(statement.body)
        is AST.Statement.While -> findAllLabeledStatements(statement.body)
        is AST.Statement.For -> findAllLabeledStatements(statement.body)
        is AST.Statement.Break -> emptyList()
        is AST.Statement.BreakLoop -> emptyList()
        is AST.Statement.BreakSwitch -> emptyList()
        is AST.Statement.Continue -> emptyList()
        is AST.Statement.Case -> findAllLabeledStatements(statement.body)
        is AST.Statement.Default -> findAllLabeledStatements(statement.body)
        is AST.Statement.Switch -> findAllLabeledStatements(statement.body)
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
    program.functionDeclarations.flatMap {
        it.body?.let(::findAllGotos) ?: emptyList()
    }

private fun findAllGotos(block: AST.Block): List<AST.Statement.Goto> =
    block.blockItems
        .flatMap(::findAllGotos)

private fun findAllGotos(blockItem: AST.BlockItem): List<AST.Statement.Goto> =
    when (blockItem) {
        is AST.BlockItem.Declaration -> emptyList()
        is AST.BlockItem.Statement -> findAllGotos(blockItem.statement)
    }

private fun findAllGotos(statement: AST.Statement): List<AST.Statement.Goto> =
    when (statement) {
        is AST.Statement.Expression -> emptyList()
        is AST.Statement.Goto -> listOf(statement)
        is AST.Statement.If -> findAllGotos(statement.thenStatement) + (statement.elseStatement?.let(::findAllGotos) ?: emptyList())
        is AST.Statement.Labeled -> findAllGotos(statement.statement)
        is AST.Statement.Null -> emptyList()
        is AST.Statement.Return -> emptyList()
        is AST.Statement.Compound -> findAllGotos(statement.block)
        is AST.Statement.DoWhile -> findAllGotos(statement.body)
        is AST.Statement.While -> findAllGotos(statement.body)
        is AST.Statement.For -> findAllGotos(statement.body)
        is AST.Statement.Break -> emptyList()
        is AST.Statement.BreakLoop -> emptyList()
        is AST.Statement.BreakSwitch -> emptyList()
        is AST.Statement.Continue -> emptyList()
        is AST.Statement.Case -> findAllGotos(statement.body)
        is AST.Statement.Default -> findAllGotos(statement.body)
        is AST.Statement.Switch -> findAllGotos(statement.body)
    }

private class LoopAndSwitchResolver {
    private var loopIdCount: Int = 0
    private var switchIdCount: Int = 0

    fun resolveLoopsAndSwitches(program: AST.Program): Either<SemanticAnalysisError, AST.Program> =
        either {
            val newProgram = program.copy(
                functionDeclarations = program.functionDeclarations.map { resolveFunctionDeclaration(it).bind() },
            )
            newProgram
        }

    private fun resolveFunctionDeclaration(
        functionDefinition: AST.Declaration.Function,
    ): Either<SemanticAnalysisError, AST.Declaration.Function> =
        either {
            functionDefinition.copy(body = functionDefinition.body?.let { resolveBlock(it, EmptyContext).bind() })
        }

    private fun resolveBlock(
        block: AST.Block,
        context: CurrentContext,
    ): Either<SemanticAnalysisError, AST.Block> =
        either {
            block.copy(blockItems = block.blockItems.map { resolveBlockItem(it, context).bind() })
        }

    private fun resolveBlockItem(blockItem: AST.BlockItem, context: CurrentContext): Either<SemanticAnalysisError, AST.BlockItem> =
        either {
            when (blockItem) {
                is AST.BlockItem.Declaration -> blockItem
                is AST.BlockItem.Statement -> AST.BlockItem.Statement(resolveStatement(blockItem.statement, context).bind())
            }
        }

    private fun resolveStatement(statement: AST.Statement, context: CurrentContext): Either<SemanticAnalysisError, AST.Statement> =
        either {
            when (statement) {
                is AST.Statement.Return -> statement
                is AST.Statement.Expression -> statement
                is AST.Statement.Null -> statement
                is AST.Statement.If -> {
                    AST.Statement.If(
                        condition = statement.condition,
                        thenStatement = resolveStatement(statement.thenStatement, context).bind(),
                        elseStatement = statement.elseStatement?.let { resolveStatement(it, context).bind() },
                    )
                }
                is AST.Statement.Labeled -> {
                    AST.Statement.Labeled(
                        label = statement.label,
                        statement = resolveStatement(statement.statement, context).bind(),
                        location = statement.location,
                    )
                }
                is AST.Statement.Goto -> statement
                is AST.Statement.Compound -> {
                    AST.Statement.Compound(resolveBlock(statement.block, context).bind())
                }
                is AST.Statement.DoWhile -> {
                    val contextWithLoop = context.withNextLoop()
                    AST.Statement.DoWhile(
                        body = resolveStatement(statement.body, contextWithLoop).bind(),
                        condition = statement.condition,
                        loopId = contextWithLoop.loopId,
                        location = statement.location,
                    )
                }
                is AST.Statement.While -> {
                    val contextWithLoop = context.withNextLoop()
                    AST.Statement.While(
                        condition = statement.condition,
                        body = resolveStatement(statement.body, contextWithLoop).bind(),
                        loopId = contextWithLoop.loopId,
                        location = statement.location,
                    )
                }
                is AST.Statement.For -> {
                    val contextWithLoop = context.withNextLoop()
                    AST.Statement.For(
                        initializer = statement.initializer,
                        condition = statement.condition,
                        post = statement.post,
                        body = resolveStatement(statement.body, contextWithLoop).bind(),
                        loopId = contextWithLoop.loopId,
                        location = statement.location,
                    )
                }
                is AST.Statement.Break -> {
                    when (context) {
                        is EmptyContext -> raise(SemanticAnalysisError("break statement outside of loop", statement.location))
                        is ContextWithLoop -> AST.Statement.BreakLoop(
                            loopId = context.loopId,
                            location = statement.location,
                        )
                        is ContextWithSwitch -> AST.Statement.BreakSwitch(
                            switchId = context.switchContext.id,
                            location = statement.location,
                        )
                    }
                }
                is AST.Statement.Continue -> {
                    ensureNotNull(context.loopId) {
                        SemanticAnalysisError("continue statement outside of loop", statement.location)
                    }
                    AST.Statement.Continue(
                        loopId = context.loopId,
                        location = statement.location,
                    )
                }

                is AST.Statement.Case -> {
                    val switchContext = ensureNotNull(context.switchContext) {
                        SemanticAnalysisError("case outside of switch", statement.location)
                    }
                    val caseId = AST.CaseId(switchContext.caseExpressions.size)

                    ensure(statement.expression is AST.Expression.IntLiteral) {
                        SemanticAnalysisError("case expression must be an integer constant", statement.location)
                    }

                    ensure(!switchContext.caseExpressions.containsKey(statement.expression.value)) {
                        SemanticAnalysisError("duplicate case expression: ${statement.expression.value}", statement.location)
                    }

                    switchContext.caseExpressions[statement.expression.value] = caseId
                    statement.copy(
                        body = resolveStatement(statement.body, context).bind(),
                        switchId = switchContext.id,
                        caseId = caseId,
                    )
                }
                is AST.Statement.Default -> {
                    val switchContext = ensureNotNull(context.switchContext) {
                        SemanticAnalysisError("default outside of switch", statement.location)
                    }
                    ensure(!switchContext.hasDefault) {
                        SemanticAnalysisError("duplicate default case", statement.location)
                    }
                    switchContext.hasDefault = true
                    statement.copy(
                        body = resolveStatement(statement.body, context).bind(),
                        switchId = switchContext.id,
                    )
                }
                is AST.Statement.Switch -> {
                    val contextWithSwitch = context.withNextSwitch()
                    val body = resolveStatement(statement.body, contextWithSwitch).bind()
                    AST.Statement.Switch(
                        expression = statement.expression,
                        body = body,
                        caseExpressions = contextWithSwitch.switchContext.caseExpressions,
                        hasDefault = contextWithSwitch.switchContext.hasDefault,
                        switchId = contextWithSwitch.switchContext.id,
                        location = statement.location,
                    )
                }
                is AST.Statement.BreakLoop -> error("Do not expect BreakLoop after parsing")
                is AST.Statement.BreakSwitch -> error("Do not expect BreakSwitch after parsing")
            }
        }

    private data class SwitchContext(
        val id: AST.SwitchId,
        val caseExpressions: MutableMap<Int, AST.CaseId>,
        var hasDefault: Boolean,
    )

    private sealed interface CurrentContext {
        val loopId: AST.LoopId?
        val switchContext: SwitchContext?
    }

    private data object EmptyContext : CurrentContext {
        override val loopId: AST.LoopId? = null
        override val switchContext: SwitchContext? = null
    }

    private data class ContextWithLoop(
        override val loopId: AST.LoopId,
        override val switchContext: SwitchContext?,
    ) : CurrentContext

    private data class ContextWithSwitch(
        override val switchContext: SwitchContext,
        override val loopId: AST.LoopId?,
    ) : CurrentContext

    private fun CurrentContext.withNextSwitch() =
        ContextWithSwitch(
            switchContext = SwitchContext(
                id = AST.SwitchId(switchIdCount++),
                caseExpressions = mutableMapOf(),
                hasDefault = false,
            ),
            loopId = loopId,
        )

    private fun CurrentContext.withNextLoop() =
        ContextWithLoop(
            loopId = AST.LoopId(loopIdCount++),
            switchContext = switchContext,
        )
}
