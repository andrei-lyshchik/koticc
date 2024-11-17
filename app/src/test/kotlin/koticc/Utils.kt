package koticc

import arrow.core.Either
import kotlin.test.fail

fun parseInput(input: String): Either<ParserError, AST.Program> {
    val tokens =
        when (val lexerResult = lexer(input)) {
            is Either.Left -> fail("lexer error: ${lexerResult.value.message()}")
            is Either.Right -> lexerResult.value
        }
    return parse(tokens)
}
