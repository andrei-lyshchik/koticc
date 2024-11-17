package koticc

import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsProvider
import java.util.stream.Stream

open class VarargArgumentsProvider(
    vararg arguments: Any,
) : ArgumentsProvider {
    private val arguments =
        arguments.map {
            when (it) {
                is Pair<*, *> -> Arguments.of(it.first, it.second)
                is Arguments -> it
                else -> Arguments.of(it)
            }
        }

    override fun provideArguments(extensionContext: ExtensionContext): Stream<out Arguments> = arguments.stream()
}
