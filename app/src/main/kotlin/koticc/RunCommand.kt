package koticc

import arrow.core.Either

fun runCommand(
    command: String,
    vararg args: String,
): Either<Throwable, CommandResult> =
    Either.catch {
        val processBuilder = ProcessBuilder(command, *args)
        val process = processBuilder.start()
        val stdout = process.inputStream.bufferedReader().use { it.readText() }
        val stderr = process.errorStream.bufferedReader().use { it.readText() }
        val exitCode = process.waitFor()
        CommandResult(exitCode, stdout, stderr)
    }
