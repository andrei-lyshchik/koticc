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

typealias TypedIdentifiers = Map<String, TypedIdentifier>

fun empty(): TypedIdentifiers = emptyMap()

fun TypedIdentifiers.functionType(functionName: String): TypedIdentifier.Function =
    this[functionName] as? TypedIdentifier.Function
        ?: error("Bug: function $functionName should have been typed during semantic analysis, got $this")

fun TypedIdentifiers.variableType(variableName: String): TypedIdentifier.Variable =
    variableTypeOrNull(variableName)
        ?: error("Bug: variable $variableName should have been typed during semantic analysis, got $this")

fun TypedIdentifiers.variableTypeOrNull(variableName: String): TypedIdentifier.Variable? =
    this[variableName]?.let { it as TypedIdentifier.Variable }
