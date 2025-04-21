package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.raise.ensureNotNull
import koticc.ast.AST
import koticc.token.Location

internal data class IdentifierResolverResult(
    val program: AST.Program,
    val renamedVariableCount: Int,
    // from new unique name to original name
    val nameMapping: Map<String, String>,
)

internal class IdentifierResolver {
    private var renamedVariableCount = 0
    private val nameMapping: MutableMap<String, String> = mutableMapOf()

    fun resolveProgram(program: AST.Program): Either<SemanticAnalysisError, IdentifierResolverResult> = either {
        val identifierMapping = mutableMapOf<String, DeclaredIdentifier>()
        val validProgram =
            program.copy(
                declarations = program.declarations.map {
                    resolveFileLevelDeclaration(it, identifierMapping).bind()
                },
            )
        IdentifierResolverResult(validProgram, renamedVariableCount, nameMapping)
    }

    private fun resolveFileLevelDeclaration(
        declaration: AST.Declaration,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Declaration> = either {
        when (declaration) {
            is AST.Declaration.Function -> resolveFunctionDeclaration(declaration, identifierMapping).bind()
            is AST.Declaration.Variable -> resolveFileLevelVariableDeclaration(declaration, identifierMapping).bind()
        }
    }

    private fun resolveFileLevelVariableDeclaration(
        declaration: AST.Declaration.Variable,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> = either {
        identifierMapping[declaration.name] = DeclaredIdentifier(
            name = declaration.name,
            hasLinkage = true,
            declaredInThisScope = true,
            location = declaration.location,
        )
        // don't resolve the initializer, as it must be a constant (if not, typechecking would return an error later)
        declaration
    }

    private fun resolveFunctionDeclaration(
        functionDeclaration: AST.Declaration.Function,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Declaration.Function> = either {
        declareFunction(functionDeclaration.name, functionDeclaration.location, identifierMapping).bind()
        val identifierMappingCopy = identifierMapping.copyForNestedBlock()
        val parameters = functionDeclaration.parameters.map { parameter ->
            val newName = declareVariable(parameter.name, parameter.location, null, identifierMappingCopy).bind()
            parameter.copy(name = newName)
        }
        functionDeclaration.copy(
            parameters = parameters,
            body = functionDeclaration.body?.let { resolveBlock(it, identifierMapping = identifierMappingCopy).bind() },
        )
    }

    private fun resolveBlock(
        block: AST.Block,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Block> = either {
        block.copy(blockItems = block.blockItems.map { resolveBlockItem(it, identifierMapping).bind() })
    }

    private fun resolveBlockItem(blockItem: AST.BlockItem, identifierMapping: MutableMap<String, DeclaredIdentifier>): Either<SemanticAnalysisError, AST.BlockItem> = either {
        when (blockItem) {
            is AST.BlockItem.Declaration -> when (blockItem.declaration) {
                is AST.Declaration.Variable ->
                    AST.BlockItem.Declaration(resolveVariableDeclaration(blockItem.declaration, identifierMapping).bind())

                is AST.Declaration.Function -> {
                    ensure(blockItem.declaration.body == null) {
                        SemanticAnalysisError(
                            "nested function declaration '${blockItem.declaration.name}' with body",
                            blockItem.declaration.location,
                        )
                    }
                    ensure(blockItem.declaration.storageClass != AST.StorageClass.Static) {
                        SemanticAnalysisError(
                            "nested function declaration '${blockItem.declaration.name}' with static storage class",
                            blockItem.declaration.location,
                        )
                    }
                    AST.BlockItem.Declaration(
                        resolveFunctionDeclaration(blockItem.declaration, identifierMapping).bind(),
                    )
                }
            }

            is AST.BlockItem.Statement ->
                AST.BlockItem.Statement(resolveStatement(blockItem.statement, identifierMapping).bind())
        }
    }

    private fun resolveVariableDeclaration(
        declaration: AST.Declaration.Variable,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Declaration.Variable> = either {
        val newName = declareVariable(declaration.name, declaration.location, declaration.storageClass, identifierMapping).bind()
        declaration.copy(name = newName, initializer = declaration.initializer?.let { resolveVariableInitializer(it, identifierMapping).bind() })
    }

    private fun resolveVariableInitializer(
        initializer: AST.VariableInitializer,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.VariableInitializer> = either {
        when (initializer) {
            is AST.VariableInitializer.Compound -> {
                initializer.copy(
                    initializers = initializer.initializers.map { initializer ->
                        resolveVariableInitializer(initializer, identifierMapping).bind()
                    },
                )
            }
            is AST.VariableInitializer.Single -> {
                initializer.copy(expression = resolveExpression(initializer.expression, identifierMapping).bind())
            }
        }
    }

    private fun declareVariable(
        originalName: String,
        location: Location,
        storageClass: AST.StorageClass?,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, String> = either {
        val previousDeclaration = identifierMapping[originalName]
        if (previousDeclaration != null && previousDeclaration.declaredInThisScope) {
            ensure(previousDeclaration.hasLinkage && storageClass == AST.StorageClass.Extern) {
                raise(alreadyDeclaredError(originalName = originalName, previousLocation = previousDeclaration.location, location = location))
            }
        }
        if (storageClass == AST.StorageClass.Extern) {
            identifierMapping[originalName] = DeclaredIdentifier(
                name = originalName,
                hasLinkage = true,
                declaredInThisScope = true,
                location = location,
            )
            originalName
        } else {
            val newName = "$originalName.${renamedVariableCount++}"
            nameMapping[newName] = originalName
            identifierMapping[originalName] = DeclaredIdentifier(
                name = newName,
                hasLinkage = false,
                declaredInThisScope = true,
                location = location,
            )
            newName
        }
    }

    private fun alreadyDeclaredError(
        originalName: String,
        previousLocation: Location,
        location: Location,
    ) = SemanticAnalysisError(
        "'$originalName' already declared at" +
            " ${previousLocation.toDisplayString()}",
        location,
    )

    private fun declareFunction(
        name: String,
        location: Location,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, String> = either {
        identifierMapping[name]?.takeIf { it.declaredInThisScope && !it.hasLinkage }?.let {
            raise(alreadyDeclaredError(originalName = name, previousLocation = it.location, location = location))
        }
        identifierMapping[name] = DeclaredIdentifier(
            name = name,
            hasLinkage = true,
            declaredInThisScope = true,
            location = location,
        )
        name
    }

    private fun resolveStatement(statement: AST.Statement, identifierMapping: MutableMap<String, DeclaredIdentifier>): Either<SemanticAnalysisError, AST.Statement> = either {
        when (statement) {
            is AST.Statement.Return ->
                AST.Statement.Return(
                    expression = resolveExpression(statement.expression, identifierMapping).bind(),
                    location = statement.location,
                )

            is AST.Statement.Expression ->
                AST.Statement.Expression(
                    expression = resolveExpression(statement.expression, identifierMapping).bind(),
                )

            is AST.Statement.Null -> statement
            is AST.Statement.If -> {
                AST.Statement.If(
                    condition = resolveExpression(statement.condition, identifierMapping).bind(),
                    thenStatement = resolveStatement(statement.thenStatement, identifierMapping).bind(),
                    elseStatement = statement.elseStatement?.let { resolveStatement(it, identifierMapping).bind() },
                )
            }

            is AST.Statement.Labeled -> {
                AST.Statement.Labeled(
                    label = statement.label,
                    statement = resolveStatement(statement.statement, identifierMapping).bind(),
                    location = statement.location,
                )
            }

            is AST.Statement.Goto -> statement
            is AST.Statement.Compound -> {
                val identifierMappingCopy = identifierMapping.copyForNestedBlock()
                AST.Statement.Compound(resolveBlock(statement.block, identifierMappingCopy).bind())
            }

            is AST.Statement.DoWhile -> {
                AST.Statement.DoWhile(
                    body = resolveStatement(statement.body, identifierMapping).bind(),
                    condition = resolveExpression(statement.condition, identifierMapping).bind(),
                    loopId = statement.loopId,
                    location = statement.location,
                )
            }

            is AST.Statement.While -> AST.Statement.While(
                condition = resolveExpression(statement.condition, identifierMapping).bind(),
                body = resolveStatement(statement.body, identifierMapping).bind(),
                loopId = statement.loopId,
                location = statement.location,
            )

            is AST.Statement.For -> {
                val headerIdentifierMapping = identifierMapping.copyForNestedBlock()
                val initializer = when (statement.initializer) {
                    is AST.ForInitializer.Declaration ->
                        AST.ForInitializer.Declaration(
                            resolveVariableDeclaration(statement.initializer.declaration, headerIdentifierMapping).bind(),
                        )

                    is AST.ForInitializer.Expression ->
                        AST.ForInitializer.Expression(
                            resolveExpression(statement.initializer.expression, headerIdentifierMapping).bind(),
                        )

                    null -> null
                }
                val condition = statement.condition?.let { resolveExpression(it, headerIdentifierMapping).bind() }
                val post = statement.post?.let { resolveExpression(it, headerIdentifierMapping).bind() }
                val body = resolveStatement(statement.body, headerIdentifierMapping).bind()
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
                expression = resolveExpression(statement.expression, identifierMapping).bind(),
                body = resolveStatement(statement.body, identifierMapping).bind(),
            )

            is AST.Statement.Default -> statement.copy(
                body = resolveStatement(statement.body, identifierMapping).bind(),
            )

            is AST.Statement.Switch -> statement.copy(
                expression = resolveExpression(statement.expression, identifierMapping).bind(),
                body = resolveStatement(statement.body, identifierMapping).bind(),
                caseExpressions = statement.caseExpressions?.mapKeys { (key, _) -> key },
            )

            is AST.Statement.BreakLoop -> statement
            is AST.Statement.BreakSwitch -> statement
        }
    }

    private fun resolveExpression(expression: AST.Expression, identifierMapping: MutableMap<String, DeclaredIdentifier>): Either<SemanticAnalysisError, AST.Expression> = either {
        when (expression) {
            is AST.Expression.Constant -> expression
            is AST.Expression.Variable -> {
                val newName = identifierMapping[expression.name]
                ensureNotNull(newName) {
                    SemanticAnalysisError("undeclared variable '${expression.toDisplayString()}'", expression.location)
                }
                AST.Expression.Variable(newName.name, null, expression.location)
            }

            is AST.Expression.Unary -> {
                AST.Expression.Unary(
                    operator = expression.operator,
                    operand = resolveExpression(expression.operand, identifierMapping).bind(),
                    type = null,
                    location = expression.location,
                )
            }

            is AST.Expression.Binary -> {
                AST.Expression.Binary(
                    operator = expression.operator,
                    left = resolveExpression(expression.left, identifierMapping).bind(),
                    right = resolveExpression(expression.right, identifierMapping).bind(),
                    type = null,
                )
            }

            is AST.Expression.Assignment -> {
                resolveAssignment(expression, identifierMapping).bind()
            }

            is AST.Expression.CompoundAssignment -> {
                val left = resolveExpression(expression.left, identifierMapping).bind()
                val right = resolveExpression(expression.right, identifierMapping).bind()

                expression.copy(left = left, right = right)
            }

            is AST.Expression.Postfix -> {
                resolvePostfix(expression, identifierMapping).bind()
            }

            is AST.Expression.Conditional -> {
                AST.Expression.Conditional(
                    condition = resolveExpression(expression.condition, identifierMapping).bind(),
                    thenExpression = resolveExpression(expression.thenExpression, identifierMapping).bind(),
                    elseExpression = resolveExpression(expression.elseExpression, identifierMapping).bind(),
                    type = null,
                )
            }

            is AST.Expression.FunctionCall -> {
                val identifier = identifierMapping[expression.name]
                ensureNotNull(identifier) {
                    SemanticAnalysisError("undeclared function '${expression.name}'", expression.location)
                }
                expression.copy(
                    name = identifier.name,
                    arguments = expression.arguments.map { resolveExpression(it, identifierMapping).bind() },
                )
            }

            is AST.Expression.Cast -> {
                expression.copy(
                    expression = resolveExpression(expression.expression, identifierMapping).bind(),
                )
            }

            is AST.Expression.AddressOf -> {
                expression.copy(
                    expression = resolveExpression(expression.expression, identifierMapping).bind(),
                )
            }
            is AST.Expression.Dereference -> {
                expression.copy(
                    expression = resolveExpression(expression.expression, identifierMapping).bind(),
                )
            }
            is AST.Expression.Subscript -> {
                expression.copy(
                    expression = resolveExpression(expression.expression, identifierMapping).bind(),
                    index = resolveExpression(expression.index, identifierMapping).bind(),
                )
            }
        }
    }

    private fun resolveAssignment(
        assignment: AST.Expression.Assignment,
        identifierMapping: MutableMap<String, DeclaredIdentifier>,
    ): Either<SemanticAnalysisError, AST.Expression.Assignment> = either {
        val left = resolveExpression(assignment.left, identifierMapping).bind()
        val right = resolveExpression(assignment.right, identifierMapping).bind()

        AST.Expression.Assignment(left, right, null)
    }

    private fun resolvePostfix(postfix: AST.Expression.Postfix, identifierMapping: MutableMap<String, DeclaredIdentifier>): Either<SemanticAnalysisError, AST.Expression.Postfix> = either {
        val operand =
            resolveExpression(postfix.operand, identifierMapping = identifierMapping).bind()
        AST.Expression.Postfix(postfix.operator, operand, null)
    }

    private data class DeclaredIdentifier(
        val name: String,
        val hasLinkage: Boolean,
        val declaredInThisScope: Boolean,
        val location: Location,
    )

    private fun MutableMap<String, DeclaredIdentifier>.copyForNestedBlock() = mapValues { (_, value) -> value.copy(declaredInThisScope = false) }.toMutableMap()
}
