package koticc

object AST {
    data class Program(
        val functionDeclarations: List<Declaration.Function>,
    ) : LocationAware {
        override val location: Location = functionDeclarations.firstOrNull()?.location ?: Location(0, 0)
    }

    data class Block(
        val blockItems: List<BlockItem>,
    ) : LocationAware {
        override val location: Location
            get() = blockItems.firstOrNull()?.location ?: Location(0, 0)
    }

    sealed interface BlockItem : LocationAware {
        data class Declaration(val declaration: AST.Declaration) : BlockItem, LocationAware {
            override val location: Location = declaration.location
        }

        data class Statement(val statement: AST.Statement) : BlockItem, LocationAware {
            override val location: Location = statement.location
        }
    }

    sealed interface Declaration : LocationAware {
        data class Variable(
            val name: String,
            val initializer: Expression?,
            override val location: Location,
        ) : Declaration

        data class Function(
            val name: String,
            val parameters: List<String>,
            val body: Block?,
            override val location: Location,
        ) : Declaration
    }

    sealed interface Statement : LocationAware {
        data class Return(val expression: AST.Expression, override val location: Location) : Statement

        data class Expression(val expression: AST.Expression) : Statement {
            override val location: Location
                get() = expression.location
        }

        data class Null(override val location: Location) : Statement

        data class If(
            val condition: AST.Expression,
            val thenStatement: Statement,
            val elseStatement: Statement?,
        ) : Statement {
            override val location: Location
                get() = condition.location
        }

        data class Labeled(
            val label: LabelName,
            val statement: Statement,
            override val location: Location,
        ) : Statement

        data class Goto(
            val label: LabelName,
            override val location: Location,
        ) : Statement

        data class Compound(
            val block: Block,
        ) : Statement {
            override val location: Location
                get() = block.location
        }

        data class DoWhile(
            val body: Statement,
            val condition: AST.Expression,
            // this is null after parsing, and filled in during semantic analysis
            val loopId: LoopId?,
            override val location: Location,
        ) : Statement

        data class While(
            val condition: AST.Expression,
            val body: Statement,
            // this is null after parsing, and filled in during semantic analysis
            val loopId: LoopId?,
            override val location: Location,
        ) : Statement

        data class For(
            val initializer: ForInitializer?,
            val condition: AST.Expression?,
            val post: AST.Expression?,
            val body: Statement,
            // this is null after parsing, and filled in during semantic analysis
            val loopId: LoopId?,
            override val location: Location,
        ) : Statement

        // Would only exist after parsing, and would be transformed either to BreakLoop or to BreakSwitch during
        // semantic analysis
        data class Break(
            override val location: Location,
        ) : Statement

        data class BreakLoop(
            val loopId: LoopId,
            override val location: Location,
        ) : Statement

        data class BreakSwitch(
            val switchId: SwitchId,
            override val location: Location,
        ) : Statement

        data class Continue(
            // null after parsing, will be filled in during semantic analysis
            val loopId: LoopId?,
            override val location: Location,
        ) : Statement

        data class Switch(
            val expression: AST.Expression,
            val body: Statement,
            override val location: Location,
            val switchId: SwitchId?,
            val caseExpressions: Map<Int, CaseId>?,
            val hasDefault: Boolean,
        ) : Statement

        data class Case(
            val expression: AST.Expression,
            val body: Statement,
            override val location: Location,
            // null after parsing, will be filled in during semantic analysis
            val switchId: SwitchId?,
            val caseId: CaseId?,
        ) : Statement

        data class Default(
            val body: Statement,
            override val location: Location,
            val switchId: SwitchId?,
        ) : Statement
    }

    @JvmInline
    value class LoopId(val value: Int)

    @JvmInline
    value class SwitchId(val value: Int)

    @JvmInline
    value class CaseId(val value: Int)

    sealed interface ForInitializer : LocationAware {
        data class Declaration(val declaration: AST.Declaration.Variable) : ForInitializer {
            override val location: Location
                get() = declaration.location
        }

        data class Expression(val expression: AST.Expression) : ForInitializer {
            override val location: Location
                get() = expression.location
        }
    }

    sealed interface Expression : LocationAware {
        data class IntLiteral(val value: Int, override val location: Location) : Expression

        data class Variable(val name: String, override val location: Location) : Expression

        data class Unary(val operator: UnaryOperator, val operand: Expression, override val location: Location) :
            Expression

        data class Binary(
            val operator: BinaryOperator,
            val left: Expression,
            val right: Expression,
        ) : Expression {
            override val location: Location
                get() = left.location
        }

        data class Assignment(val left: Expression, val right: Expression) :
            Expression {
            override val location: Location
                get() = left.location
        }

        data class CompoundAssignment(
            val operator: CompoundAssignmentOperator,
            val left: Expression,
            val right: Expression,
        ) : Expression {
            override val location: Location
                get() = left.location
        }

        data class Postfix(
            val operator: PostfixOperator,
            val operand: Expression,
        ) : Expression {
            override val location: Location
                get() = operand.location
        }

        data class Conditional(
            val condition: Expression,
            val thenExpression: Expression,
            val elseExpression: Expression,
        ) : Expression {
            override val location: Location
                get() = condition.location
        }

        data class FunctionCall(
            val name: String,
            val arguments: List<Expression>,
            override val location: Location,
        ) : Expression
    }

    enum class UnaryOperator {
        Negate,
        Complement,
        LogicalNegate,
    }

    enum class BinaryOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
        Equal,
        NotEqual,
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        LogicalAnd,
        LogicalOr,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        ShiftLeft,
        ShiftRight,
    }

    enum class CompoundAssignmentOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        ShiftLeft,
        ShiftRight,
    }

    enum class PostfixOperator {
        Increment,
        Decrement,
    }
}
