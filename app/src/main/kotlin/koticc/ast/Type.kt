package koticc.ast

import koticc.common.Displayable

sealed interface Type : Displayable {
    sealed interface Data : Type {
        fun signed(): Boolean
        fun size(): kotlin.Int
    }

    data object Int : Data {
        override fun toDisplayString(): String = "int"

        override fun signed(): Boolean = true

        override fun size(): kotlin.Int = 4
    }

    data object UInt : Data {
        override fun toDisplayString(): String = "unsigned int"

        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = 4
    }

    data object Long : Data {
        override fun toDisplayString(): String = "long"

        override fun signed(): Boolean = true

        override fun size(): kotlin.Int = 8
    }

    data object ULong : Data {
        override fun toDisplayString(): String = "unsigned long"

        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = 8
    }

    data class Function(
        val parameters: List<Data>,
        val returnType: Data,
    ) : Type {
        override fun toDisplayString(): String = "function(${parameters.joinToString { it.toDisplayString() }}): ${returnType.toDisplayString()}"
    }
}
