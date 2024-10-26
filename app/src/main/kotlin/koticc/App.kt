package koticc

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.groups.mutuallyExclusiveOptions
import com.github.ajalt.clikt.parameters.groups.single
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.path
import kotlin.io.path.nameWithoutExtension
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    CompilerCommand().main(args)
}

class CompilerCommand : CliktCommand(name = "ktc") {
    private val partialMode: PartialMode? by
        mutuallyExclusiveOptions(
            option(
                "--lex",
                help =
                    "Run only lexer, and stop after that, producing no output files, " +
                        "and returning non-zero exit code if any of the stages fail.",
            ).flag()
                .convert { PartialMode.Lex },
            option(
                "--parse",
                help = "Run only parser, and stop after that, producing no output files",
            ).flag()
                .convert { PartialMode.Parse },
            option(
                "--validate",
                help =
                    "Run only semantic validation, and stop after that, producing no output files",
            ).flag()
                .convert { PartialMode.Validate },
            option(
                "--tacky",
                help =
                    "Run only tacky code generation, and stop after that, producing no output files",
            ).flag()
                .convert { PartialMode.Tacky },
            option(
                "--codegen",
                help =
                    "Run only assembly code generation, and stop after that, producing no output files",
            ).flag()
                .convert { PartialMode.Codegen },
            option(
                "--emit-assembly",
                "-S",
                help =
                    "Emit assembly into output file, but do not run the assembler and linker",
            ).flag()
                .convert { PartialMode.EmitAssembly },
        ).single()

    private val inputFile by argument(help = "Input file to compile").path()

    override fun run() {
        val outputFile = inputFile.resolveSibling(inputFile.nameWithoutExtension)
        runCompilerDriver(inputFile, partialMode, outputFile)
            .fold(
                ifLeft = { error ->
                    echo("Error: ${error.message()}")
                    exitProcess(1)
                },
                ifRight = {},
            )
    }
}
