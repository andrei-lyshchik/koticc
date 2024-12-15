package koticc.semantic

sealed interface Symbol {
    data class Variable(
        val type: Type.Data,
        val attributes: VariableAttributes,
    ) : Symbol

    data class Function(
        val type: Type.Function,
        val defined: Boolean,
        val global: Boolean,
    ) : Symbol
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

typealias SymbolTable = Map<String, Symbol>

fun empty(): SymbolTable = emptyMap()

fun SymbolTable.functionSymbol(functionName: String): Symbol.Function =
    this[functionName] as? Symbol.Function
        ?: error("Bug: function $functionName should have been added to the symbol table during semantic analysis, got $this")

fun SymbolTable.variableSymbol(variableName: String): Symbol.Variable =
    variableSymbolOrNull(variableName)
        ?: error("Bug: variable $variableName should have been added to the symbol table during semantic analysis, got $this")

fun SymbolTable.variableSymbolOrNull(variableName: String): Symbol.Variable? =
    this[variableName]?.let { it as Symbol.Variable }
