@file:Suppress("ktlint:standard:function-naming")

package koticc.ast

fun program(block: ProgramBuilder.() -> Unit): AST.Program {
    val builder = ProgramBuilder()
    builder.block()
    return builder.build()
}

class ProgramBuilder {
    private var declarations: MutableList<AST.Declaration> = mutableListOf()
    private var currentVariableBuilder: VariableDeclarationBuilder? = null

    private fun addDeclaration(declaration: AST.Declaration) {
        currentVariableBuilder?.let {
            declarations.add(it.buildVariableDeclaration())
            currentVariableBuilder = null
        }
        declarations.add(declaration)
    }

    private fun setCurrentVariableBuilder(variableBuilder: VariableDeclarationBuilder): VariableDeclarationBuilder {
        currentVariableBuilder?.let {
            declarations.add(it.buildVariableDeclaration())
        }
        currentVariableBuilder = variableBuilder
        return variableBuilder
    }

    fun function(name: String, type: Type.Function, vararg parameters: String, block: (BlockBuilder.() -> Unit)? = null) {
        val body = if (block != null) {
            BlockBuilder().apply(block).build()
        } else {
            null
        }
        addDeclaration(
            AST.Declaration.Function(
                name,
                parameters.map { AST.FunctionParameter(it, DUMMY_LOCATION) },
                body,
                type,
                null,
                DUMMY_LOCATION,
            ),
        )
    }

    fun function(name: String, type: Type.Function, storageClass: AST.StorageClass, vararg parameters: String, block: (BlockBuilder.() -> Unit)? = null) {
        val body = if (block != null) {
            BlockBuilder().apply(block).build()
        } else {
            null
        }
        addDeclaration(
            AST.Declaration.Function(
                name,
                parameters.map { AST.FunctionParameter(it, DUMMY_LOCATION) },
                body,
                type,
                storageClass,
                DUMMY_LOCATION,
            ),
        )
    }

    fun int(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentVariableBuilder(VariableDeclarationBuilder(name, storageClass, Type.Int))

    fun uInt(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentVariableBuilder(VariableDeclarationBuilder(name, storageClass, Type.UInt))

    fun long(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentVariableBuilder(VariableDeclarationBuilder(name, storageClass, Type.Long))

    fun double(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentVariableBuilder(VariableDeclarationBuilder(name, storageClass, Type.Double))

    fun build(): AST.Program {
        currentVariableBuilder?.let {
            declarations.add(it.buildVariableDeclaration())
        }
        return AST.Program(declarations)
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

    fun int(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentBlockItemBuilder(VariableDeclarationBuilder(name, storageClass, Type.Int))

    fun uInt(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentBlockItemBuilder(VariableDeclarationBuilder(name, storageClass, Type.UInt))

    fun long(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentBlockItemBuilder(VariableDeclarationBuilder(name, storageClass, Type.Long))

    fun uLong(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentBlockItemBuilder(VariableDeclarationBuilder(name, storageClass, Type.ULong))

    fun double(name: String, storageClass: AST.StorageClass? = null): VariableDeclarationBuilder =
        setCurrentBlockItemBuilder(VariableDeclarationBuilder(name, storageClass, Type.Double))

    fun assign(left: AST.Expression, right: AST.Expression, type: Type.Data? = null) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Expression(
                    AST.Expression.Assignment(
                        left = left,
                        right = right,
                        type = type,
                    ),
                ),
            ),
        )
    }

    fun plusAssign(
        left: AST.Expression,
        right: AST.Expression,
        resultType: Type.Data? = null,
        rightSideType: Type.Data? = null,
    ) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Expression(
                    compoundAssignment(
                        operator = AST.BinaryOperator.Add,
                        left = left,
                        right = right,
                        resultType = resultType,
                        rightSideType = rightSideType,
                    ),
                ),
            ),
        )
    }

    fun plusMultiply(
        left: AST.Expression,
        right: AST.Expression,
        resultType: Type.Data? = null,
        rightSideType: Type.Data? = null,
    ) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Expression(
                    compoundAssignment(
                        operator = AST.BinaryOperator.Multiply,
                        left = left,
                        right = right,
                        resultType = resultType,
                        rightSideType = rightSideType,
                    ),
                ),
            ),
        )
    }

    fun function(name: String, type: Type.Function, vararg parameters: String, block: (BlockBuilder.() -> Unit)? = null) {
        val body = if (block != null) {
            BlockBuilder().apply(block).build()
        } else {
            null
        }
        addBlockItem(
            AST.BlockItem.Declaration(
                AST.Declaration.Function(
                    name,
                    parameters.map { AST.FunctionParameter(it, DUMMY_LOCATION) },
                    body,
                    type,
                    null,
                    DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun function(name: String, type: Type.Function, storageClass: AST.StorageClass, vararg parameters: String, block: (BlockBuilder.() -> Unit)? = null) {
        val body = if (block != null) {
            BlockBuilder().apply(block).build()
        } else {
            null
        }
        addBlockItem(
            AST.BlockItem.Declaration(
                AST.Declaration.Function(
                    name,
                    parameters.map { AST.FunctionParameter(it, DUMMY_LOCATION) },
                    body,
                    type,
                    storageClass,
                    DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun return_(expression: AST.Expression) {
        addBlockItem(AST.BlockItem.Statement(AST.Statement.Return(expression, DUMMY_LOCATION)))
    }

    fun call(functionName: String, vararg arguments: AST.Expression, type: Type.Data? = null) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Expression(
                    AST.Expression.FunctionCall(
                        name = functionName,
                        arguments = arguments.toList(),
                        location = DUMMY_LOCATION,
                        type = type,
                    ),
                ),
            ),
        )
    }

    fun if_(condition: AST.Expression, thenBlock: BlockBuilder.() -> Unit): IfBuilder {
        return setCurrentBlockItemBuilder(IfBuilder(condition, thenBlock))
    }

    fun do_(block: BlockBuilder.() -> Unit) =
        setCurrentBlockItemBuilder(DoWhileBuilder(BlockBuilder().apply(block).build()))

    fun while_(condition: AST.Expression, loopId: Int? = null, block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.While(
                    condition = condition,
                    body = AST.Statement.Compound(BlockBuilder().apply(block).build()),
                    loopId = loopId?.let(AST::LoopId),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun for_(initializer: AST.ForInitializer?, condition: AST.Expression?, post: AST.Expression?, loopId: Int? = null, block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.For(
                    initializer = initializer,
                    condition = condition,
                    post = post,
                    body = AST.Statement.Compound(BlockBuilder().apply(block).build()),
                    loopId = loopId?.let(AST::LoopId),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun break_() {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Break(
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun breakLoop(loopId: Int) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.BreakLoop(
                    loopId = AST.LoopId(loopId),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun breakSwitch(switchId: Int) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.BreakSwitch(
                    switchId = AST.SwitchId(switchId),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun continue_(loopId: Int? = null) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Continue(
                    loopId = loopId?.let(AST::LoopId),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun switch(
        expression: AST.Expression,
        switchId: Int? = null,
        caseExpressions: Map<AST.Constant, Int>? = null,
        hasDefault: Boolean = false,
        block: BlockBuilder.() -> Unit,
    ) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Switch(
                    expression = expression,
                    body = AST.Statement.Compound(BlockBuilder().apply(block).build()),
                    location = DUMMY_LOCATION,
                    switchId = switchId?.let(AST::SwitchId),
                    caseExpressions = caseExpressions?.mapValues { AST.CaseId(it.value) },
                    hasDefault = hasDefault,
                ),
            ),
        )
    }

    fun case(
        expression: AST.Expression,
        caseId: Int? = null,
        switchId: Int? = null,
        block: BlockBuilder.() -> Unit,
    ) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Case(
                    expression = expression,
                    body = BlockBuilder().apply(block).build().toSingleStatement(),
                    location = DUMMY_LOCATION,
                    caseId = caseId?.let(AST::CaseId),
                    switchId = switchId?.let(AST::SwitchId),
                ),
            ),
        )
    }

    fun default(
        switchId: Int? = null,
        block: BlockBuilder.() -> Unit,
    ) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Default(
                    body = BlockBuilder().apply(block).build().toSingleStatement(),
                    location = DUMMY_LOCATION,
                    switchId = switchId?.let(AST::SwitchId),
                ),
            ),
        )
    }

    fun nested(block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Compound(
                    BlockBuilder().apply(block).build(),
                ),
            ),
        )
    }

    fun goto(label: String) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Goto(
                    label = LabelName(label),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun label(name: String, block: BlockBuilder.() -> Unit) {
        addBlockItem(
            AST.BlockItem.Statement(
                AST.Statement.Labeled(
                    label = LabelName(name),
                    statement = BlockBuilder().apply(block).build().toSingleStatement(),
                    location = DUMMY_LOCATION,
                ),
            ),
        )
    }

    fun null_() {
        addBlockItem(AST.BlockItem.Statement(AST.Statement.Null(DUMMY_LOCATION)))
    }

    fun build(): AST.Block {
        currentBlockItemBuilder?.let {
            blockItems.add(it.build())
        }
        return AST.Block(blockItems)
    }
}

fun AST.Block.toSingleStatement(): AST.Statement =
    when {
        blockItems.size == 1 -> when (val blockItem = blockItems[0]) {
            is AST.BlockItem.Statement -> blockItem.statement
            else -> AST.Statement.Compound(this)
        }
        else -> AST.Statement.Compound(this)
    }

interface BlockItemBuilder {
    fun build(): AST.BlockItem
}

class VariableDeclarationBuilder(private val name: String, private val storageClass: AST.StorageClass?, private val type: Type.Data) : BlockItemBuilder {
    private var initializer: AST.Expression? = null

    infix fun assign(initializer: AST.Expression) {
        this.initializer = initializer
    }

    fun buildVariableDeclaration(): AST.Declaration.Variable {
        return AST.Declaration.Variable(name, initializer, type, storageClass, DUMMY_LOCATION)
    }

    override fun build(): AST.BlockItem = AST.BlockItem.Declaration(
        AST.Declaration.Variable(
            name,
            initializer,
            type,
            storageClass,
            DUMMY_LOCATION,
        ),
    )
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
    private var loopId: Int? = null

    fun while_(condition: AST.Expression, loopId: Int? = null) {
        this.condition = condition
        this.loopId = loopId
    }

    override fun build(): AST.BlockItem {
        return AST.BlockItem.Statement(
            AST.Statement.DoWhile(
                body = AST.Statement.Compound(body),
                condition = condition ?: error("No condition"),
                loopId = loopId?.let(AST::LoopId),
                location = DUMMY_LOCATION,
            ),
        )
    }
}

fun initDecl(name: String, initializer: AST.Expression? = null) =
    AST.ForInitializer.Declaration(AST.Declaration.Variable(name, initializer, Type.Int, null, DUMMY_LOCATION))

fun initExpr(expression: AST.Expression) =
    AST.ForInitializer.Expression(expression)

val Int.e
    get() = AST.Expression.Constant(AST.IntConstant(this), null, DUMMY_LOCATION)

val UInt.e
    get() = AST.Expression.Constant(AST.UIntConstant(this), null, DUMMY_LOCATION)

val Int.c
    get() = AST.IntConstant(this)

val Long.e
    get() = AST.Expression.Constant(AST.LongConstant(this), null, DUMMY_LOCATION)

val ULong.e
    get() = AST.Expression.Constant(AST.ULongConstant(this), null, DUMMY_LOCATION)

val Double.e
    get() = AST.Expression.Constant(AST.DoubleConstant(this), null, DUMMY_LOCATION)

val Double.c
    get() = AST.DoubleConstant(this)

val Long.c
    get() = AST.LongConstant(this)

val String.e
    get() = AST.Expression.Variable(this, null, DUMMY_LOCATION)

fun cast(type: Type.Data, expression: AST.Expression) =
    AST.Expression.Cast(expression, type, null, DUMMY_LOCATION)

operator fun String.invoke(vararg args: AST.Expression): AST.Expression {
    return AST.Expression.FunctionCall(
        name = this,
        arguments = args.toList(),
        location = DUMMY_LOCATION,
        type = null,
    )
}

infix fun AST.Expression.assign(other: AST.Expression): AST.Expression {
    return AST.Expression.Assignment(
        left = this,
        right = other,
        type = null,
    )
}

operator fun AST.Expression.plus(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Add,
        left = this,
        right = other,
        type = null,
    )
}

operator fun AST.Expression.unaryMinus(): AST.Expression {
    return AST.Expression.Unary(
        operator = AST.UnaryOperator.Negate,
        operand = this,
        type = null,
        location = DUMMY_LOCATION,
    )
}

operator fun AST.Expression.not(): AST.Expression {
    return AST.Expression.Unary(
        operator = AST.UnaryOperator.LogicalNegate,
        operand = this,
        type = null,
        location = DUMMY_LOCATION,
    )
}

fun AST.Expression.complement(): AST.Expression {
    return AST.Expression.Unary(
        operator = AST.UnaryOperator.Complement,
        operand = this,
        type = null,
        location = DUMMY_LOCATION,
    )
}

infix fun AST.Expression.plusAssign(other: AST.Expression): AST.Expression {
    return compoundAssignment(
        operator = AST.BinaryOperator.Add,
        left = this,
        right = other,
        resultType = type,
        rightSideType = type,
    )
}

infix fun AST.Expression.plusMultiply(other: AST.Expression): AST.Expression {
    return compoundAssignment(
        operator = AST.BinaryOperator.Multiply,
        left = this,
        right = other,
        resultType = null,
    )
}

infix fun AST.Expression.gt(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.GreaterThan,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.eq(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Equal,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.minus(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Subtract,
        left = this,
        right = other,
        type = null,
    )
}

infix operator fun AST.Expression.times(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Multiply,
        left = this,
        right = other,
        type = null,
    )
}

infix operator fun AST.Expression.div(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Divide,
        left = this,
        right = other,
        type = null,
    )
}

infix operator fun AST.Expression.rem(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Modulo,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.and(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LogicalAnd,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.or(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LogicalOr,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.eq(other: Int): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.Equal,
        left = this,
        right = AST.Expression.Constant(AST.IntConstant(other), null, DUMMY_LOCATION),
        type = null,
    )
}

infix fun AST.Expression.lt(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LessThan,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.lte(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.LessThanOrEqual,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.gte(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.GreaterThanOrEqual,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.ne(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.NotEqual,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.shl(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.ShiftLeft,
        left = this,
        right = other,
        type = null,
    )
}

infix fun AST.Expression.xor(other: AST.Expression): AST.Expression {
    return AST.Expression.Binary(
        operator = AST.BinaryOperator.BitwiseXor,
        left = this,
        right = other,
        type = null,
    )
}

fun AST.Expression.postfixIncrement(): AST.Expression {
    return AST.Expression.Postfix(
        operator = AST.PostfixOperator.Increment,
        operand = this,
        type = null,
    )
}

fun cond(condition: AST.Expression, thenExpression: AST.Expression, elseExpression: AST.Expression) =
    AST.Expression.Conditional(
        condition = condition,
        thenExpression = thenExpression,
        elseExpression = elseExpression,
        type = null,
    )

fun AST.Expression.int() = ofType(Type.Int)

fun AST.Expression.long() = ofType(Type.Long)

fun AST.Expression.uInt() = ofType(Type.UInt)

fun AST.Expression.uLong() = ofType(Type.ULong)

fun AST.Expression.double() = ofType(Type.Double)

fun compoundAssignment(
    left: AST.Expression,
    right: AST.Expression,
    operator: AST.BinaryOperator,
    resultType: Type.Data? = null,
    rightSideType: Type.Data? = null,
) =
    AST.Expression.Assignment(
        left = left,
        right = AST.Expression.Binary(
            operator = operator,
            left = left,
            right = right,
            type = rightSideType,
        ),
        type = resultType,
    )
