package koticc.semantic

sealed interface Type {
    sealed interface Data : Type

    data object Integer : Data

    data class Function(
        val parameterCount: Int,
    ) : Type
}
