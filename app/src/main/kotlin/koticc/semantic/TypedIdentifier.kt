package koticc.semantic

sealed interface TypedIdentifier {
    data class Variable(
        val type: Type.Data,
        val attributes: VariableAttributes,
    ) : TypedIdentifier

    data class Function(
        val type: Type.Function,
        val defined: Boolean,
        val global: Boolean,
    ) : TypedIdentifier
}

sealed interface VariableAttributes {
    val global: Boolean
    val initialValue: InitialValue?

    data class Static(
        override val initialValue: InitialValue,
        override val global: Boolean,
    ) : VariableAttributes

    data object Local : VariableAttributes {
        override val global: Boolean = false
        override val initialValue: InitialValue? = null
    }
}

sealed interface InitialValue {
    data object Tentative : InitialValue
    data class Constant(val value: Int) : InitialValue
    data object NoInitializer : InitialValue
}
