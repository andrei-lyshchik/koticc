package koticc.ast

import koticc.common.Displayable

sealed interface Type : Displayable {
    sealed interface Data : Type {
        fun signed(): Boolean
        fun size(): kotlin.Int
    }

    sealed interface Arithmetic : Data

    data object Int : Arithmetic {
        override fun toDisplayString(): String = "int"

        override fun signed(): Boolean = true

        override fun size(): kotlin.Int = 4
    }

    data object UInt : Arithmetic {
        override fun toDisplayString(): String = "unsigned int"

        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = 4
    }

    data object Long : Arithmetic {
        override fun toDisplayString(): String = "long"

        override fun signed(): Boolean = true

        override fun size(): kotlin.Int = 8
    }

    data object ULong : Arithmetic {
        override fun toDisplayString(): String = "unsigned long"

        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = 8
    }

    data object Double : Arithmetic {
        override fun toDisplayString(): String = "double"

        override fun signed(): Boolean = true

        override fun size(): kotlin.Int = 8
    }

    data class Pointer(
        val referenced: Data,
    ) : Data {
        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = 8

        override fun toDisplayString(): String = "${referenced.toDisplayString()} *"
    }

    data class Array(
        val type: Data,
        val size: kotlin.Int,
    ) : Data {
        override fun signed(): Boolean = false

        override fun size(): kotlin.Int = type.size() * size

        override fun toDisplayString(): String = "${type.toDisplayString()} [$size]"
    }

    data class Function(
        val parameters: List<Data>,
        val returnType: Data,
    ) : Type {
        override fun toDisplayString(): String = "function(${parameters.joinToString { it.toDisplayString() }}): ${returnType.toDisplayString()}"
    }
}
