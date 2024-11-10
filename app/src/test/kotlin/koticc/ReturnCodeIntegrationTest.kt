package koticc

import arrow.core.raise.either
import arrow.core.raise.ensure
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import java.io.File
import java.nio.file.Path
import kotlin.io.path.absolutePathString
import kotlin.test.assertEquals
import kotlin.test.fail

class ReturnCodeIntegrationTest {
    companion object {
        @TempDir
        lateinit var tempDir: Path
    }

    class TestCases : VarargArgumentsProvider(
        "/programs/binary.c",
        "/programs/bitwise.c",
        "/programs/comparisons.c",
        "/programs/compound_assignment.c",
        "/programs/logical.c",
        "/programs/simple_variables.c",
        "/programs/unary.c",
        "/programs/increments_decrements.c",
        "/programs/if.c",
        "/programs/conditional_ternary.c",
        "/programs/labels.c",
    )

    @ParameterizedTest
    @ArgumentsSource(TestCases::class)
    fun `should produce correct binary that returns same exit code as gcc`(path: String) {
        val inputFile = tempDir.resolve("input.c").toFile()
        copyResourceToFile(path, inputFile)

        val gccBinaryExitCode = getGccProducedBinaryExitCode(inputFile)
        val ktcBinaryExitCode = getKtcProducedBinaryExitCode(inputFile)

        assertEquals(gccBinaryExitCode, ktcBinaryExitCode)
    }

    private fun getGccProducedBinaryExitCode(inputFile: File) =
        either {
            val gccResult =
                runCommand("gcc", "-o", tempDir.resolve("gcc_output").absolutePathString(), inputFile.absolutePath)
                    .mapLeft { "GCC failed: $it" }
                    .bind()
            ensure(gccResult.exitCode == 0) { "GCC failed with exit code ${gccResult.exitCode}: ${gccResult.stderr}" }
            val gccBinaryResult =
                runCommand(tempDir.resolve("gcc_output").absolutePathString())
                    .mapLeft { "GCC produced binary failed to run: $it" }
                    .bind()
            gccBinaryResult.exitCode
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { exitCode -> exitCode }

    private fun getKtcProducedBinaryExitCode(inputFile: File) =
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
            runCompilerDriver(
                inputFile = inputFile.toPath(),
                partialMode = null,
                outputFile = tempDir.resolve("ktc_output"),
            ).mapLeft { "ktc failed: ${it.message()}" }.bind()
            val ktcBinaryResult =
                runCommand(tempDir.resolve("ktc_output").absolutePathString())
                    .mapLeft { "ktc produced binary failed to run: $it" }
                    .bind()
            ktcBinaryResult.exitCode
        }.fold(
            ifLeft = {
                fail(it)
            },
        ) { exitCode -> exitCode }

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
