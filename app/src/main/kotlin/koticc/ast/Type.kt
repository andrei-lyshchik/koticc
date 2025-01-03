package koticc.ast

sealed interface Type {
    sealed interface Data : Type

    data object Int : Data

    data object Long : Data

    data class Function(
        val parameters: List<Data>,
        val returnType: Data,
    ) : Type
}
