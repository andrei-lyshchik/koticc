package koticc.ast

import koticc.common.Displayable

sealed interface Type : Displayable {
    sealed interface Data : Type

    data object Int : Data {
        override fun toDisplayString(): String = "int"
    }

    data object Long : Data {
        override fun toDisplayString(): String = "long"
    }

    data class Function(
        val parameters: List<Data>,
        val returnType: Data,
    ) : Type {
        override fun toDisplayString(): String = "function(${parameters.joinToString { it.toDisplayString() }}): ${returnType.toDisplayString()}"
    }
}
