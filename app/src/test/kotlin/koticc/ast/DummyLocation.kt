package koticc.ast

import arrow.core.Either
import arrow.core.right
import koticc.CompilerError
import koticc.token.Location
import kotlin.test.assertEquals

val DUMMY_LOCATION = Location(0, 0)

fun assertEqualsIgnoringLocations(expected: AST.Program, actual: Either<CompilerError, AST.Program>) {
    val expectedWithDummyLocations = expected.withDummyLocations()
    val actualWithDummyLocations = actual.map { it.withDummyLocations() }
    assertEquals(expectedWithDummyLocations.right(), actualWithDummyLocations)
}

fun AST.Program.withDummyLocations() = copy(
    declarations = declarations.map {
        when (it) {
            is AST.Declaration.Function -> it.withDummyLocations()
            is AST.Declaration.Variable -> it.withDummyLocations()
        }
    },
)

fun AST.Declaration.Function.withDummyLocations() = copy(
    location = DUMMY_LOCATION,
    body = body?.withDummyLocations(),
    parameters = parameters.map { it.withDummyLocations() },
)

fun AST.FunctionParameter.withDummyLocations() = copy(location = DUMMY_LOCATION)

fun AST.Block.withDummyLocations() = copy(
    blockItems = blockItems.map { it.withDummyLocations() },
)

fun AST.BlockItem.withDummyLocations(): AST.BlockItem = when (this) {
    is AST.BlockItem.Declaration -> when (val declaration = this.declaration) {
        is AST.Declaration.Variable -> copy(declaration = declaration.withDummyLocations())
        is AST.Declaration.Function -> copy(declaration = declaration.withDummyLocations())
    }
    is AST.BlockItem.Statement -> copy(statement = statement.withDummyLocations())
}

fun AST.Declaration.Variable.withDummyLocations() = copy(
    location = DUMMY_LOCATION,
    initializer = initializer?.withDummyLocations(),
)

fun AST.Statement.withDummyLocations(): AST.Statement = when (this) {
    is AST.Statement.Return -> copy(expression = expression.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.Expression -> copy(expression = expression.withDummyLocations())
    is AST.Statement.Break -> copy(location = DUMMY_LOCATION)
    is AST.Statement.Compound -> copy(block = block.withDummyLocations())
    is AST.Statement.Continue -> copy(location = DUMMY_LOCATION)
    is AST.Statement.DoWhile -> copy(body = body.withDummyLocations(), condition = condition.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.For -> copy(initializer = initializer?.withDummyLocations(), body = body.withDummyLocations(), condition = condition?.withDummyLocations(), post = post?.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.Goto -> copy(location = DUMMY_LOCATION)
    is AST.Statement.If -> copy(condition = condition.withDummyLocations(), thenStatement = thenStatement.withDummyLocations(), elseStatement = elseStatement?.withDummyLocations())
    is AST.Statement.Labeled -> copy(statement = statement.withDummyLocations())
    is AST.Statement.Null -> copy(location = DUMMY_LOCATION)
    is AST.Statement.While -> copy(body = body.withDummyLocations(), condition = condition.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.Case -> copy(expression = expression.withDummyLocations(), body = body.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.Default -> copy(body = body.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.Switch -> copy(body = body.withDummyLocations(), expression = expression.withDummyLocations(), location = DUMMY_LOCATION)
    is AST.Statement.BreakLoop -> copy(location = DUMMY_LOCATION)
    is AST.Statement.BreakSwitch -> copy(location = DUMMY_LOCATION)
}

fun AST.ForInitializer.withDummyLocations(): AST.ForInitializer =
    when (this) {
        is AST.ForInitializer.Expression -> copy(expression = expression.withDummyLocations())
        is AST.ForInitializer.Declaration -> copy(declaration = declaration.withDummyLocations())
    }

fun AST.Expression.withDummyLocations(): AST.Expression = when (this) {
    is AST.Expression.Constant -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Variable -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Binary -> copy(left = left.withDummyLocations(), right = right.withDummyLocations())
    is AST.Expression.Unary -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Assignment -> copy(left = left.withDummyLocations(), right = right.withDummyLocations())
    is AST.Expression.Conditional -> copy(condition = condition.withDummyLocations(), thenExpression = thenExpression.withDummyLocations(), elseExpression = elseExpression.withDummyLocations())
    is AST.Expression.Postfix -> copy(operand = operand.withDummyLocations())
    is AST.Expression.FunctionCall -> copy(arguments = arguments.map { it.withDummyLocations() }, location = DUMMY_LOCATION)
    is AST.Expression.Cast -> copy(expression = expression.withDummyLocations(), location = DUMMY_LOCATION)
}
