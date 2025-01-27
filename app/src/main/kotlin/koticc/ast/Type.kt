package koticc.ast

import koticc.common.Displayable

sealed interface Type : Displayable {
    sealed interface Data : Type

    data object Int : Data {
        override fun toDisplayString(): String = "int"
    }

    data object UInt : Data {
        override fun toDisplayString(): String = "unsigned int"
    }

    data object Long : Data {
        override fun toDisplayString(): String = "long"
    }

    data object ULong : Data {
        override fun toDisplayString(): String = "unsigned long"
    }

    data class Function(
        val parameters: List<Data>,
        val returnType: Data,
    ) : Type {
        override fun toDisplayString(): String = "function(${parameters.joinToString { it.toDisplayString() }}): ${returnType.toDisplayString()}"
    }
}
