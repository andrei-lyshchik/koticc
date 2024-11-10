package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
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

        val assemblyProgram = tackyProgramToAssembly(tackyProgram)
        if (partialMode == PartialMode.Codegen) {
            return@either
        }

        val assemblyFile = inputFile.resolveSibling(inputFile.nameWithoutExtension + ".s")
        writeAssemblyToFile(assemblyProgram, assemblyFile).bind()
        if (partialMode == PartialMode.EmitAssembly) {
            return@either
        }

        runLinkerAndAssembler(assemblyFile, outputFile).bind()

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

fun runLinkerAndAssembler(
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
            ).mapLeft { LinkerAndAssemblerError("failed to run linker and assembler: ${it.message}") }
                .bind()

        ensure(result.exitCode == 0) {
            LinkerAndAssemblerError(
                "linker and assembler failed with exit code ${result.exitCode}, output: ${result.stderr}",
            )
        }
    }
