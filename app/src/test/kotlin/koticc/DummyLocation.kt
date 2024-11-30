package koticc

import arrow.core.Either
import arrow.core.right
import kotlin.test.assertEquals

val DUMMY_LOCATION = Location(0, 0)

fun assertEqualsIgnoringLocations(expected: AST.Program, actual: Either<CompilerError, AST.Program>) {
    val expectedWithDummyLocations = expected.withDummyLocations()
    val actualWithDummyLocations = actual.map { it.withDummyLocations() }
    assertEquals(expectedWithDummyLocations.right(), actualWithDummyLocations)
}

fun AST.Program.withDummyLocations() = copy(
    functionDefinition = functionDefinition.withDummyLocations(),
)

fun AST.FunctionDefinition.withDummyLocations() = copy(
    location = DUMMY_LOCATION,
    body = body.withDummyLocations(),
)

fun AST.Block.withDummyLocations() = copy(
    blockItems = blockItems.map { it.withDummyLocations() },
)

fun AST.BlockItem.withDummyLocations() = when (this) {
    is AST.BlockItem.Declaration -> copy(declaration = declaration.withDummyLocations())
    is AST.BlockItem.Statement -> copy(statement = statement.withDummyLocations())
}

fun AST.Declaration.withDummyLocations() = copy(
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
    is AST.Expression.IntLiteral -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Variable -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Binary -> copy(left = left.withDummyLocations(), right = right.withDummyLocations())
    is AST.Expression.Unary -> copy(location = DUMMY_LOCATION)
    is AST.Expression.Assignment -> copy(left = left.withDummyLocations(), right = right.withDummyLocations())
    is AST.Expression.CompoundAssignment -> copy(left = left.withDummyLocations(), right = right.withDummyLocations())
    is AST.Expression.Conditional -> copy(condition = condition.withDummyLocations(), thenExpression = thenExpression.withDummyLocations(), elseExpression = elseExpression.withDummyLocations())
    is AST.Expression.Postfix -> copy(operand = operand.withDummyLocations())
}