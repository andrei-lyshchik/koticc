package koticc.ast

import koticc.common.Displayable
import koticc.token.Location
import koticc.token.LocationAware

object AST {
    data class Program(
        val declarations: List<Declaration>,
    ) : LocationAware {
        override val location: Location = declarations.firstOrNull()?.location ?: Location(0, 0)
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
            val type: Type.Data,
            val storageClass: StorageClass?,
            override val location: Location,
        ) : Declaration

        data class Function(
            val name: String,
            val parameters: List<FunctionParameter>,
            val body: Block?,
            val type: Type.Function,
            val storageClass: StorageClass?,
            override val location: Location,
        ) : Declaration {
            init {
                require(parameters.size == type.parameters.size) {
                    "Bug: function $name has ${parameters.size} parameters, but its type has ${type.parameters.size} parameters"
                }
            }
        }
    }

    enum class StorageClass : Displayable {
        Extern,
        Static,
        ;

        override fun toDisplayString(): String = name.lowercase()
    }

    data class FunctionParameter(
        val name: String,
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
            val caseExpressions: Map<Constant, CaseId>?,
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

    sealed interface Expression : LocationAware, Displayable {
        // at parse time this would be null, and would be filled in during semantic analysis
        val type: Type.Data?

        fun resolvedType() = type ?: error("Bug: type not resolved during semantic analysis in $this")

        fun ofType(type: Type.Data): Expression

        data class Constant(
            val value: AST.Constant,
            override val type: Type.Data?,
            override val location: Location,
        ) : Expression {
            override fun toDisplayString(): String = value.toDisplayString()

            override fun ofType(type: Type.Data): Constant = copy(type = type)
        }

        data class Variable(
            val name: String,
            override val type: Type.Data?,
            override val location: Location,
        ) : Expression {
            override fun toDisplayString(): String = name

            override fun ofType(type: Type.Data): Variable = copy(type = type)
        }

        data class Unary(
            val operator: UnaryOperator,
            val operand: Expression,
            override val type: Type.Data?,
            override val location: Location,
        ) : Expression {
            override fun toDisplayString(): String = "${operator.toDisplayString()}${operand.toDisplayString()}"

            override fun ofType(type: Type.Data): Unary = copy(type = type)
        }

        data class Binary(
            val operator: BinaryOperator,
            val left: Expression,
            val right: Expression,
            override val type: Type.Data?,
        ) : Expression {
            override val location: Location
                get() = left.location

            override fun toDisplayString(): String = "${left.toDisplayString()} ${operator.toDisplayString()} ${right.toDisplayString()}"

            override fun ofType(type: Type.Data): Binary = copy(type = type)
        }

        data class Assignment(
            val left: Expression,
            val right: Expression,
            override val type: Type.Data?,
        ) : Expression {
            override val location: Location
                get() = left.location

            override fun toDisplayString(): String = "${left.toDisplayString()} = ${right.toDisplayString()}"

            override fun ofType(type: Type.Data): Assignment = copy(type = type)
        }

        data class CompoundAssignment(
            val operator: CompoundAssignmentOperator,
            val left: Expression,
            val right: Expression,
            override val type: Type.Data?,
        ) : Expression {
            override val location: Location
                get() = left.location

            override fun toDisplayString(): String = "${left.toDisplayString()} ${operator.toDisplayString()} ${right.toDisplayString()}"

            override fun ofType(type: Type.Data): CompoundAssignment = copy(type = type)
        }

        data class Postfix(
            val operator: PostfixOperator,
            val operand: Expression,
            override val type: Type.Data?,
        ) : Expression {
            override val location: Location
                get() = operand.location

            override fun toDisplayString(): String = "${operand.toDisplayString()}${operator.toDisplayString()}"

            override fun ofType(type: Type.Data): Postfix = copy(type = type)
        }

        data class Conditional(
            val condition: Expression,
            val thenExpression: Expression,
            val elseExpression: Expression,
            override val type: Type.Data?,
        ) : Expression {
            override val location: Location
                get() = condition.location

            override fun toDisplayString(): String = "${condition.toDisplayString()} ? ${thenExpression.toDisplayString()} : ${elseExpression.toDisplayString()}"

            override fun ofType(type: Type.Data): Conditional = copy(type = type)
        }

        data class FunctionCall(
            val name: String,
            val arguments: List<Expression>,
            override val location: Location,
            override val type: Type.Data?,
        ) : Expression {
            override fun toDisplayString(): String = "$name(${arguments.joinToString(", ") { it.toDisplayString() }})"

            override fun ofType(type: Type.Data): FunctionCall = copy(type = type)
        }

        data class Cast(
            val expression: Expression,
            val targetType: Type.Data,
            override val type: Type.Data?,
            override val location: Location,
        ) : Expression {
            override fun toDisplayString(): String = "(${targetType.toDisplayString()}) ${expression.toDisplayString()}"

            override fun ofType(type: Type.Data): Expression = copy(type = type)
        }
    }

    sealed interface Constant : Displayable

    data class IntConstant(val value: Int) : Constant {
        override fun toDisplayString(): String = value.toString()
    }

    data class LongConstant(val value: Long) : Constant {
        override fun toDisplayString(): String = value.toString()
    }

    enum class UnaryOperator : Displayable {
        Negate,
        Complement,
        LogicalNegate,
        ;

        override fun toDisplayString(): String = when (this) {
            Negate -> "-"
            Complement -> "~"
            LogicalNegate -> "!"
        }
    }

    enum class BinaryOperator : Displayable {
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
        ;

        override fun toDisplayString(): String = when (this) {
            Add -> "+"
            Subtract -> "-"
            Multiply -> "*"
            Divide -> "/"
            Modulo -> "%"
            Equal -> "=="
            NotEqual -> "!="
            LessThan -> "<"
            LessThanOrEqual -> "<="
            GreaterThan -> ">"
            GreaterThanOrEqual -> ">="
            LogicalAnd -> "&&"
            LogicalOr -> "||"
            BitwiseAnd -> "&"
            BitwiseOr -> "|"
            BitwiseXor -> "^"
            ShiftLeft -> "<<"
            ShiftRight -> ">>"
        }
    }

    enum class CompoundAssignmentOperator : Displayable {
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
        ;

        override fun toDisplayString(): String = when (this) {
            Add -> "+="
            Subtract -> "-="
            Multiply -> "*="
            Divide -> "/="
            Modulo -> "%="
            BitwiseAnd -> "&="
            BitwiseOr -> "|="
            BitwiseXor -> "^="
            ShiftLeft -> "<<="
            ShiftRight -> ">>="
        }
    }

    enum class PostfixOperator : Displayable {
        Increment,
        Decrement,
        ;

        override fun toDisplayString(): String = when (this) {
            Increment -> "++"
            Decrement -> "--"
        }
    }
}
