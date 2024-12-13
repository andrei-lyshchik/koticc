package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.raise.ensureNotNull
import koticc.ast.AST

internal class LoopAndSwitchResolver {
    private var loopIdCount: Int = 0
    private var switchIdCount: Int = 0

    fun resolveLoopsAndSwitches(program: AST.Program): Either<SemanticAnalysisError, AST.Program> =
        either {
            val newProgram = program.copy(
                declarations = program.declarations.map { resolveDeclaration(it).bind() },
            )
            newProgram
        }

    private fun resolveDeclaration(declaration: AST.Declaration): Either<SemanticAnalysisError, AST.Declaration> =
        either {
            when (declaration) {
                is AST.Declaration.Function -> resolveFunctionDeclaration(declaration).bind()
                is AST.Declaration.Variable -> declaration
            }
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
