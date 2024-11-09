package koticc

object AST {
    data class Program(
        val functionDefinition: FunctionDefinition,
    ) : LocationAware {
        override val location: Location = functionDefinition.location
    }

    data class FunctionDefinition(
        val name: String,
        val body: List<BlockItem>,
        override val location: Location,
    ) : LocationAware

    sealed interface BlockItem {
        data class Declaration(val declaration: AST.Declaration) : BlockItem, LocationAware {
            override val location: Location = declaration.location
        }

        data class Statement(val statement: AST.Statement) : BlockItem, LocationAware {
            override val location: Location = statement.location
        }
    }

    data class Declaration(
        val name: String,
        val initializer: Expression?,
        override val location: Location,
    ) : LocationAware

    sealed interface Statement : LocationAware {
        data class Return(val expression: AST.Expression, override val location: Location) : Statement

        data class Expression(val expression: AST.Expression) : Statement {
            override val location: Location
                get() = expression.location
        }

        data class Null(override val location: Location) : Statement

        data class If(
            val condition: AST.Expression,
            val thenStatement: AST.Statement,
            val elseStatement: AST.Statement?,
        ) : Statement {
            override val location: Location
                get() = condition.location
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
