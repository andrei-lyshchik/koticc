@file:Suppress("ktlint:standard:function-naming")

package koticc

fun program(block: ProgramBuilder.() -> Unit): AST.Program {
    val builder = ProgramBuilder()
    builder.block()
    return builder.build()
}

class ProgramBuilder {
    private var functionDefinition: AST.FunctionDefinition? = null

    fun functionDefinition(name: String, block: BlockBuilder.() -> Unit) {
        val blockBuilder = BlockBuilder()
        blockBuilder.block()
        functionDefinition = AST.FunctionDefinition(name, blockBuilder.build(), DUMMY_LOCATION)
    }

    fun build(): AST.Program {
        return AST.Program(functionDefinition ?: error("No function definition"))
    }
}

class BlockBuilder {
    private val blockItems = mutableListOf<AST.BlockItem>()
    private var currentBlockItemBuilder: BlockItemBuilder? = null

    private fun <T : BlockItemBuilder> setCurrentBlockItemBuilder(blockItemBuilder: T): T {
        currentBlockItemBuilder?.let {
            blockItems.add(it.build())
        }
        currentBlockItemBuilder = blockItemBuilder
        return blockItemBuilder
    }

    private fun addBlockItem(blockItem: AST.BlockItem) {
        currentBlockItemBuilder?.let {
            blockItems.add(it.build())
            currentBlockItemBuilder = null
        }
        blockItems.add(blockItem)
    }

    fun int(name: String): DeclarationBuilder =
        setCurrentBlockItemBuilder(DeclarationBuilder(name))

    fun return_(expression: AST.Expression) {
        addBlockItem(AST.BlockItem.Statement(AST.Statement.Return(expression, DUMMY_LOCATION)))
    }

    fun e(expression: AST.Expression) {
        addBlockItem(AST.BlockItem.Statement(AST.Statement.Expression(expression)))
    }

    fun if_(condition: AST.Expression, thenBlock: BlockBuilder.() -> Unit): IfBuilder {
        return setCurrentBlockItemBuilder(IfBuilder(condition, thenBlock))
    }

    fun do_(block: BlockBuilder.() -> Unit) =
        setCurrentBlockItemBuilder(DoWhileBuilder(BlockBuilder().apply(block).build()))

    fun while_(condition: AST.Expression, loopIndex: Int? = null, block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.While(
                    condition = condition,
                    body = AST.Statement.Compound(BlockBuilder().apply(block).build()),
                    continueLabel = loopIndex?.let { LabelName("while.continue.$loopIndex") },
                    breakLabel = loopIndex?.let { LabelName("while.break.$loopIndex") },
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun for_(initializer: AST.ForInitializer?, condition: AST.Expression?, post: AST.Expression?, loopIndex: Int? = null, block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.For(
                    initializer = initializer,
                    condition = condition,
                    post = post,
                    body = AST.Statement.Compound(BlockBuilder().apply(block).build()),
                    continueLabel = loopIndex?.let { LabelName("for.continue.$loopIndex") },
                    breakLabel = loopIndex?.let { LabelName("for.break.$loopIndex") },
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun break_(labelNameValue: String? = null) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Break(
                    label = labelNameValue?.let { LabelName(labelNameValue) },
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun continue_(labelNameValue: String? = null) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Continue(
                    label = labelNameValue?.let { LabelName(labelNameValue) },
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun build(): AST.Block {
        currentBlockItemBuilder?.let {
            blockItems.add(it.build())
        }
        return AST.Block(blockItems)
    }
}

interface BlockItemBuilder {
    fun build(): AST.BlockItem
}

class DeclarationBuilder(val name: String) : BlockItemBuilder {
    var initializer: AST.Expression? = null

    infix fun assign(initializer: AST.Expression) {
        this.initializer = initializer
    }

    override fun build(): AST.BlockItem = AST.BlockItem.Declaration(AST.Declaration(name, initializer, DUMMY_LOCATION))
}

class IfBuilder(val condition: AST.Expression, thenBlock: BlockBuilder.() -> Unit) : BlockItemBuilder {
    private val thenBlock = BlockBuilder().apply(thenBlock).build()
    private var elseBlock: AST.Block? = null

    infix fun else_(elseBlock: BlockBuilder.() -> Unit) {
        val elseBuilder = BlockBuilder()
        elseBuilder.elseBlock()
        this.elseBlock = elseBuilder.build()
    }

    override fun build(): AST.BlockItem {
        return AST.BlockItem.Statement(
            AST.Statement.If(
                condition = condition,
                thenStatement = AST.Statement.Compound(thenBlock),
                elseStatement = elseBlock?.let { AST.Statement.Compound(it) },
            ),
        )
    }
}

class DoWhileBuilder(val body: AST.Block) : BlockItemBuilder {
    private var condition: AST.Expression? = null
    private var loopIndex: Int? = null

    fun while_(condition: AST.Expression, labelLoopIndex: Int? = null) {
        this.condition = condition
        this.loopIndex = labelLoopIndex
    }

    override fun build(): AST.BlockItem {
        return AST.BlockItem.Statement(
            AST.Statement.DoWhile(
                body = AST.Statement.Compound(body),
                condition = condition ?: error("No condition"),
                continueLabel = loopIndex?.let { LabelName("do_while.continue.$loopIndex") },
                breakLabel = loopIndex?.let { LabelName("do_while.break.$loopIndex") },
                location = DUMMY_LOCATION,
            ),
        )
    }
}

fun initDecl(name: String, initializer: AST.Expression? = null) =
    AST.ForInitializer.Declaration(AST.Declaration(name, initializer, DUMMY_LOCATION))

fun initExpr(expression: AST.Expression) =
    AST.ForInitializer.Expression(expression)

val Int.e
    get() = AST.Expression.IntLiteral(this, DUMMY_LOCATION)

val String.e
    get() = AST.Expression.Variable(this, DUMMY_LOCATION)

infix fun AST.Expression.assign(other: AST.Expression): AST.Expression {
    return AST.Expression.Assignment(
        left = this,
        right = other,
    )
}

operator fun AST.Expression.plus(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Add,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.plusAssign(other: AST.Expression): AST.Expression {
    return AST.Expression.CompoundAssignment(
        operator = AST.CompoundAssignmentOperator.Add,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.plusMultiply(other: AST.Expression): AST.Expression {
    return AST.Expression.CompoundAssignment(
        operator = AST.CompoundAssignmentOperator.Multiply,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.gt(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.GreaterThan,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.eq(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Equal,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.minus(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Subtract,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.times(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Multiply,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.div(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Divide,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.mod(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Modulo,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.and(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.BitwiseAnd,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.or(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LogicalOr,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.eq(other: Int): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Equal,
        left = this,
        right = AST.Expression.IntLiteral(other, DUMMY_LOCATION),
    )
}

infix fun AST.Expression.lt(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LessThan,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.lte(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LessThanOrEqual,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.gte(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.GreaterThanOrEqual,
        left = this,
        right = other,
    )
}

infix fun AST.Expression.ne(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.NotEqual,
        left = this,
        right = other,
    )
}
