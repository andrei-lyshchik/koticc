package koticc.assembly

typealias BackendSymbolTable = Map<String, BackendSymbol>

sealed interface BackendSymbol {
    data class Object(
        val type: Assembly.Type,
        val static: Boolean,
    ) : BackendSymbol

    data class Function(
        val defined: Boolean,
    ) : BackendSymbol
}
