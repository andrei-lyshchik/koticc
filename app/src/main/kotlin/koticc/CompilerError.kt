package koticc

import java.io.IOException

interface CompilerError {
    fun message(): String
}

data class IOError(val message: String, val exception: IOException) : CompilerError {
    override fun message(): String = "$message: ${exception.message}"
}
