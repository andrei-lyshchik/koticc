package koticc

import arrow.core.Either
import java.io.StringWriter
import kotlin.test.assertTrue
import kotlin.test.fail

fun parseInput(input: String): Either<ParserError, AST.Program> {
    val tokens =
        when (val lexerResult = lexer(input)) {
            is Either.Left -> fail("lexer error: ${lexerResult.value.message()}")
            is Either.Right -> lexerResult.value
        }
    return parse(tokens)
}

fun tackyProgramToAssemblyString(program: Tacky.Program): String {
    val assembly = tackyProgramToAssembly(program)
    return assemblyToString(assembly)
}

fun assemblyToString(assembly: Assembly.Program): String {
    val writer = StringWriter()
    val result = writeAssemblyProgram(assembly, writer)
    assertTrue(result.isRight())
    return writer.toString()
}
