package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import koticc.assembly.Assembly
import koticc.assembly.tackyProgramToAssembly
import koticc.assembly.writeAssemblyProgram
import koticc.ast.parse
import koticc.command.runCommand
import koticc.semantic.semanticAnalysis
import koticc.tacky.programASTToTacky
import koticc.token.lexer
import java.io.BufferedWriter
import java.io.IOException
import java.nio.file.Path
import kotlin.io.path.absolutePathString
import kotlin.io.path.bufferedWriter
import kotlin.io.path.deleteIfExists
import kotlin.io.path.nameWithoutExtension

enum class PartialMode {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    EmitAssembly,
    ObjectFile,
}

fun runCompilerDriver(
    inputFile: Path,
    partialMode: PartialMode?,
    outputFile: Path,
): Either<CompilerError, Unit> =
    either {
        val preprocessed = runPreprocessor(inputFile).bind()

        val tokens = lexer(preprocessed).bind()
        if (partialMode == PartialMode.Lex) {
            return@either
        }

        val ast = parse(tokens).bind()
        if (partialMode == PartialMode.Parse) {
            return@either
        }

        val validAST = semanticAnalysis(ast).bind()
        if (partialMode == PartialMode.Validate) {
            return@either
        }

        val tackyProgram = programASTToTacky(validAST)
        if (partialMode == PartialMode.Tacky) {
            return@either
        }

        val assemblyProgram = tackyProgramToAssembly(tackyProgram, validAST.symbolTable)
        if (partialMode == PartialMode.Codegen) {
            return@either
        }

        val assemblyFile = inputFile.resolveSibling(inputFile.nameWithoutExtension + ".s")
        writeAssemblyToFile(assemblyProgram, assemblyFile).bind()
        if (partialMode == PartialMode.EmitAssembly) {
            return@either
        }

        if (partialMode == PartialMode.ObjectFile) {
            runAssembler(assemblyFile, outputFile).bind()
        } else {
            runAssemblerAndLinker(assemblyFile, outputFile).bind()
        }

        Either.catchOrThrow<IOException, Unit> { assemblyFile.deleteIfExists() }
            .mapLeft { IOError("failed to delete assembly file", it) }
            .bind()
    }

data class PreprocessorError(
    val message: String,
) : CompilerError {
    override fun message(): String = message
}

fun runPreprocessor(inputFile: Path): Either<PreprocessorError, String> =
    either {
        val result =
            runCommand(
                "arch",
                "-x86_64",
                "gcc",
                "-E",
                "-P",
                inputFile.absolutePathString(),
            )
                .mapLeft { PreprocessorError("failed to run preprocessor: ${it.message}") }
                .bind()

        ensure(result.exitCode == 0) {
            PreprocessorError(
                "preprocessor failed with exit code ${result.exitCode}, output: ${result.stderr}",
            )
        }
        result.stdout
    }

fun writeAssemblyToFile(
    assembly: Assembly.Program,
    outputFilePath: Path,
): Either<CompilerError, Unit> =
    either {
        val writer =
            Either.catchOrThrow<IOException, BufferedWriter> { outputFilePath.bufferedWriter() }
                .mapLeft { IOError("error opening assembly output file", it) }
                .bind()
        writeAssemblyProgram(assembly, writer)
            .mapLeft { IOError("error writing assembly output file", it) }
            .bind()
    }

data class LinkerAndAssemblerError(
    val message: String,
) : CompilerError {
    override fun message(): String = message
}

fun runAssemblerAndLinker(
    assemblyFilePath: Path,
    outputFile: Path,
): Either<CompilerError, Unit> =
    either {
        val result =
            runCommand(
                "arch",
                "-x86_64",
                "gcc",
                assemblyFilePath.absolutePathString(),
                "-o",
                outputFile.absolutePathString(),
            ).mapLeft { LinkerAndAssemblerError("failed to run linked and assembler: ${it.message}") }
                .bind()

        ensure(result.exitCode == 0) {
            LinkerAndAssemblerError(
                "linker and assembler failed with exit code ${result.exitCode}, output: ${result.stderr}",
            )
        }
    }

fun runAssembler(
    assemblyFilePath: Path,
    outputFile: Path,
): Either<CompilerError, Unit> =
    either {
        val result =
            runCommand(
                "arch",
                "-x86_64",
                "gcc",
                "-c",
                assemblyFilePath.absolutePathString(),
                "-o",
                outputFile.absolutePathString(),
            ).mapLeft { LinkerAndAssemblerError("failed to run assembler: ${it.message}") }
                .bind()

        ensure(result.exitCode == 0) {
            LinkerAndAssemblerError(
                "assembler failed with exit code ${result.exitCode}, output: ${result.stderr}",
            )
        }
    }
