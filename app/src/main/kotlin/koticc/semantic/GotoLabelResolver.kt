package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import koticc.ast.AST
import koticc.ast.LabelName
import koticc.token.Location

internal class GotoLabelResolver {
    fun resolveLabels(program: AST.Program): Either<SemanticAnalysisError, AST.Program> = either {
        program.copy(
            functionDeclarations = program.functionDeclarations.map { resolveFunctionDeclarationLabels(it).bind() },
        )
    }

    private fun resolveFunctionDeclarationLabels(function: AST.Declaration.Function): Either<SemanticAnalysisError, AST.Declaration.Function> = either {
        val labels = mutableMapOf<LabelName, Location>()
        val gotoLabels = mutableMapOf<LabelName, Location>()
        val result = function.copy(
            body = function.body?.let { resolveBlockLabels(it, function.name, labels = labels, gotoLabels = gotoLabels).bind() },
        )

        gotoLabels.forEach { (gotoLabel, location) ->
            if (gotoLabel !in labels) {
                raise(SemanticAnalysisError("goto to undeclared label '${gotoLabel.value}'", location))
            }
        }

        result
    }

    private fun resolveBlockLabels(
        block: AST.Block,
        functionName: String,
        labels: MutableMap<LabelName, Location>,
        gotoLabels: MutableMap<LabelName, Location>,
    ): Either<SemanticAnalysisError, AST.Block> = either {
        block.copy(
            blockItems = block.blockItems.map {
                resolveBlockItemLabels(
                    it,
                    functionName,
                    labels,
                    gotoLabels,
                ).bind()
            },
        )
    }

    private fun resolveBlockItemLabels(
        blockItem: AST.BlockItem,
        functionName: String,
        labels: MutableMap<LabelName, Location>,
        gotoLabels: MutableMap<LabelName, Location>,
    ): Either<SemanticAnalysisError, AST.BlockItem> = either {
        when (blockItem) {
            is AST.BlockItem.Declaration -> blockItem
            is AST.BlockItem.Statement -> blockItem.copy(statement = resolveStatementLabels(blockItem.statement, functionName, labels, gotoLabels).bind())
        }
    }

    private fun resolveStatementLabels(
        statement: AST.Statement,
        functionName: String,
        labels: MutableMap<LabelName, Location>,
        gotoLabels: MutableMap<LabelName, Location>,
    ): Either<SemanticAnalysisError, AST.Statement> = either {
        when (statement) {
            is AST.Statement.Break -> statement
            is AST.Statement.BreakLoop -> statement
            is AST.Statement.BreakSwitch -> statement
            is AST.Statement.Case -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )

            is AST.Statement.Compound -> statement.copy(
                block = resolveBlockLabels(statement.block, functionName, labels, gotoLabels).bind(),
            )

            is AST.Statement.Continue -> statement
            is AST.Statement.Default -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )
            is AST.Statement.DoWhile -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )
            is AST.Statement.Expression -> statement
            is AST.Statement.For -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )
            is AST.Statement.Goto -> {
                if (statement.label !in gotoLabels) {
                    gotoLabels[statement.label] = statement.location
                }
                statement.copy(
                    label = toUniqueLabelName(statement.label, functionName),
                )
            }
            is AST.Statement.If -> {
                val thenStatement = resolveStatementLabels(statement.thenStatement, functionName, labels, gotoLabels).bind()
                val elseStatement = statement.elseStatement?.let { resolveStatementLabels(it, functionName, labels, gotoLabels).bind() }
                statement.copy(
                    thenStatement = thenStatement,
                    elseStatement = elseStatement,
                )
            }
            is AST.Statement.Labeled -> {
                val existingLocation = labels[statement.label]
                if (existingLocation != null) {
                    raise(
                        SemanticAnalysisError(
                            "label '${statement.label.value}' already declared at" +
                                " ${existingLocation.toHumanReadableString()}",
                            statement.location,
                        ),
                    )
                }
                labels[statement.label] = statement.location
                statement.copy(
                    label = toUniqueLabelName(statement.label, functionName),
                    statement = resolveStatementLabels(statement.statement, functionName, labels, gotoLabels).bind(),
                )
            }
            is AST.Statement.Null -> statement
            is AST.Statement.Return -> statement
            is AST.Statement.Switch -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )
            is AST.Statement.While -> statement.copy(
                body = resolveStatementLabels(statement.body, functionName, labels, gotoLabels).bind(),
            )
        }
    }

    private fun toUniqueLabelName(originalName: LabelName, function: String) = LabelName("$function.${originalName.value}")
}
