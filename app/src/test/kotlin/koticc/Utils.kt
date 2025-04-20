package koticc

import arrow.core.Either
import arrow.core.raise.either
import koticc.assembly.Assembly
import koticc.assembly.tackyProgramToAssembly
import koticc.assembly.writeAssemblyProgram
import koticc.ast.AST
import koticc.ast.parse
import koticc.semantic.ValidASTProgram
import koticc.semantic.semanticAnalysis
import koticc.tacky.Tacky
import koticc.token.lexer
import java.io.StringWriter
import kotlin.test.assertTrue

fun parseInput(input: String): Either<CompilerError, AST.Program> = either {
    val tokens = lexer(input).bind()
    parse(tokens).bind()
}

fun parseAndAnalyze(input: String): Either<CompilerError, ValidASTProgram> = either {
    val program = parseInput(input).bind()
    semanticAnalysis(program).bind()
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

fun main() {
    printBytesRepresentation(1152921504606846977)
    printNumberForBytes("01 00 00 00 00 00 00 10")
}

private fun printBytesRepresentation(number: Long) {
    val byteArray = ByteArray(Long.SIZE_BYTES)
    for (i in byteArray.indices) {
        byteArray[i] = (number shr (i * 8) and 0xFF).toByte()
    }
    println(byteArray.joinToString(" ") { it.toUByte().toString(16).padStart(2, '0') })
}

private fun printNumberForBytes(bytes: String) {
    val byteArray = bytes.split(" ").map { it.toUByte(16).toByte() }.toByteArray()
    var result = 0L
    for (i in byteArray.indices) {
        result = result or (byteArray[i].toLong() and 0xFF shl (i * 8))
    }
    println(result)
}
