package koticc

data class CommandResult(
    val exitCode: Int,
    val stdout: String,
    val stderr: String,
)
