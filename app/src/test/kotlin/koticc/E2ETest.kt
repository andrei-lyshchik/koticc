package koticc

import arrow.core.raise.either
import arrow.core.raise.ensure
import koticc.command.CommandResult
import koticc.command.runCommand
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import java.io.File
import java.nio.file.Path
import java.util.UUID
import kotlin.io.path.absolutePathString
import kotlin.test.assertEquals
import kotlin.test.fail

class E2ETest {
    companion object {
        @TempDir
        lateinit var tempDir: Path
    }

    class TestCases : VarargArgumentsProvider(
//        "/programs/switch.c",
//        "/programs/binary.c",
//        "/programs/binary_long.c",
//        "/programs/bitwise.c",
//        "/programs/bitwise_long.c",
//        "/programs/comparisons.c",
//        "/programs/comparisons_long.c",
//        "/programs/compound_assignment.c",
//        "/programs/logical.c",
//        "/programs/simple_variables.c",
//        "/programs/unary.c",
//        "/programs/increments_decrements.c",
//        "/programs/if.c",
//        "/programs/conditional_ternary.c",
//        "/programs/labels.c",
//        "/programs/nested_blocks.c",
//        "/programs/loops.c",
//        "/programs/functions.c",
//        "/programs/sign_extend.c",
//        "/programs/sign_extend_in_assignment.c",
        "/programs/long_logical_not.c",
    )

    @ParameterizedTest
    @ArgumentsSource(TestCases::class)
    fun `should produce correct binary that returns same exit code as gcc`(path: String) {
        val inputFile = tempDir.resolve("input_${UUID.randomUUID()}.c").toFile()
        copyResourceToFile(path, inputFile)

        val gccBinaryExitCode = getGccProducedBinaryResult(inputFile)
        val ktcBinaryExitCode = getKtcProducedBinaryResult(inputFile)

        assertEquals(gccBinaryExitCode, ktcBinaryExitCode)
    }

    class MultipleFilesTestCases : VarargArgumentsProvider(
        MultipleFilesTestCase(
            file1 = "/programs/functions_client.c",
            file2ForGccOnly = "/programs/functions_impl.c",
        ),
        MultipleFilesTestCase(
            file1 = "/programs/functions_impl.c",
            file2ForGccOnly = "/programs/functions_client.c",
        ),
        MultipleFilesTestCase(
            file1 = "/programs/hello_world.c",
            file2ForGccOnly = "/programs/stdio.c",
        ),
        MultipleFilesTestCase(
            file1 = "/programs/static_impl.c",
            file2ForGccOnly = "/programs/static_client.c",
        ),
        MultipleFilesTestCase(
            file1 = "/programs/static_client.c",
            file2ForGccOnly = "/programs/static_impl.c",
        ),
        MultipleFilesTestCase(
            file1 = "/programs/factorial_long.c",
            file2ForGccOnly = "/programs/print.c",
        ),
    )

    data class MultipleFilesTestCase(
        val file1: String,
        val file2ForGccOnly: String,
    )

    @ParameterizedTest
    @ArgumentsSource(MultipleFilesTestCases::class)
    fun `should produce correct object files`(testCase: MultipleFilesTestCase) {
        val inputFile1 = tempDir.resolve("input1_${UUID.randomUUID()}.c").toFile()
        copyResourceToFile(testCase.file1, inputFile1)
        val inputFile2 = tempDir.resolve("input2_${UUID.randomUUID()}.c").toFile()
        copyResourceToFile(testCase.file2ForGccOnly, inputFile2)

        val gccObjectFile1 = getGccProducedObjectFile(inputFile1)
        val gccObjectFile2 = getGccProducedObjectFile(inputFile2)
        val ktcObjectFile1 = getKtcProducedObjectFile(inputFile1)

        val gccOnlyResult = createAndRunExecutableFile(gccObjectFile1, gccObjectFile2)
        val ktcAndGccResult = createAndRunExecutableFile(ktcObjectFile1, gccObjectFile2)

        assertEquals(gccOnlyResult, ktcAndGccResult)
    }

    private fun getGccProducedBinaryResult(inputFile: File) =
        either {
            val outputFilePath = tempDir.resolve("gcc_output_${UUID.randomUUID()}").absolutePathString()
            val gccResult =
                runCommand("gcc", "-o", outputFilePath, inputFile.absolutePath)
                    .mapLeft { "GCC failed: $it" }
                    .bind()
            ensure(gccResult.exitCode == 0) { "GCC failed with exit code ${gccResult.exitCode}: ${gccResult.stderr}" }
            val gccBinaryResult =
                runCommand(outputFilePath)
                    .mapLeft { "GCC produced binary failed to run: $it" }
                    .bind()
            gccBinaryResult
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { it }

    private fun getKtcProducedBinaryResult(inputFile: File) =
        either {
            runCompilerDriver(
                inputFile = inputFile.toPath(),
                partialMode = PartialMode.EmitAssembly,
                outputFile = tempDir.resolve("ktc_output"),
            ).mapLeft { "ktc failed to produce assembly: ${it.message()}" }.bind()
            inputFile.resolveSibling(inputFile.nameWithoutExtension + ".s").readText()
                .let { assembly ->
                    println("Assembly produced by ktc:\n$assembly")
                }
            val outputFileName = "ktc_output_${UUID.randomUUID()}"
            runCompilerDriver(
                inputFile = inputFile.toPath(),
                partialMode = null,
                outputFile = tempDir.resolve(outputFileName),
            ).mapLeft { "ktc failed: ${it.message()}" }.bind()
            val ktcBinaryResult =
                runCommand(tempDir.resolve(outputFileName).absolutePathString())
                    .mapLeft { "ktc produced binary failed to run: $it" }
                    .bind()
            ktcBinaryResult
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { it }

    private fun getGccProducedObjectFile(inputFile: File) =
        either {
            val outputFilePath = tempDir.resolve("gcc_object_file_${UUID.randomUUID()}.o").absolutePathString()
            val gccResult =
                runCommand("arch", "-x86_64", "gcc", "-c", inputFile.absolutePath, "-o", outputFilePath)
                    .mapLeft { "GCC failed: $it" }
                    .bind()
            ensure(gccResult.exitCode == 0) { "GCC failed with exit code ${gccResult.exitCode}: ${gccResult.stderr}" }
            val gccObjectFile = File(outputFilePath)
            gccObjectFile
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { it }

    private fun getKtcProducedObjectFile(inputFile: File) =
        either {
            val outputFileName = "ktc_object_file_${UUID.randomUUID()}.o"
            val outputFilePath = tempDir.resolve(outputFileName)
            runCompilerDriver(
                inputFile = inputFile.toPath(),
                partialMode = PartialMode.ObjectFile,
                outputFile = outputFilePath,
            ).mapLeft { "ktc failed: ${it.message()}" }.bind()
            val ktcObjectFile = outputFilePath.toFile()
            ktcObjectFile
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { it }

    private fun createAndRunExecutableFile(
        vararg objectFiles: File,
    ): CommandResult = either<String, CommandResult> {
        val outputFilePath = tempDir.resolve("output_${UUID.randomUUID()}").absolutePathString()
        val objectFilesPaths = objectFiles.map { it.absolutePath }
        val gccResult =
            runCommand("arch", "-x86_64", "gcc", "-o", outputFilePath, *objectFilesPaths.toTypedArray())
                .mapLeft { "GCC failed: $it" }
                .bind()
        ensure(gccResult.exitCode == 0) { "GCC failed with exit code ${gccResult.exitCode}: ${gccResult.stderr}" }
        val binaryResult =
            runCommand(outputFilePath)
                .mapLeft { "Binary failed to run: $it" }
                .bind()
        return binaryResult
    }.fold(
        ifLeft = {
            fail(it)
        },
    ) { it }

    private fun copyResourceToFile(
        resourcePath: String,
        inputFile: File,
    ) {
        val resource =
            Companion::class.java.getResourceAsStream(resourcePath)
                ?: error("Resource not found: $resourcePath")
        resource.use { inputStream ->
            inputFile.outputStream().buffered().use { outputStream ->
                inputStream.copyTo(outputStream)
            }
        }
    }
}
