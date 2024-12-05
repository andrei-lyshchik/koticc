package koticc

sealed interface Type {
    data object Integer : Type

    data class Function(
        val parameterCount: Int,
    ) : Type
}
