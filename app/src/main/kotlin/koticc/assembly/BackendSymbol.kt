package koticc.assembly

import koticc.ast.Type
import koticc.semantic.Symbol
import koticc.semantic.SymbolTable
import koticc.semantic.VariableAttributes

typealias BackendSymbolTable = Map<String, BackendSymbol>

sealed interface BackendSymbol {
    data class Object(
        val signed: Boolean,
        val type: Assembly.Type,
        val static: Boolean,
    ) : BackendSymbol

    data class Function(
        val defined: Boolean,
    ) : BackendSymbol
}

fun BackendSymbolTable.functionSymbol(functionName: String): BackendSymbol.Function =
    this[functionName] as? BackendSymbol.Function
        ?: error("Bug: function $functionName should have been added to the symbol table during semantic analysis, got $this")

fun BackendSymbolTable.objectSymbol(variableName: String): BackendSymbol.Object =
    this[variableName]?.let { it as BackendSymbol.Object }
        ?: error("Bug: variable $variableName should have been added to the symbol table during semantic analysis, got $this")

fun SymbolTable.toBackendSymbolTable() = mapValues { (_, symbol) ->
    when (symbol) {
        is Symbol.Function -> BackendSymbol.Function(symbol.defined)
        is Symbol.Variable -> BackendSymbol.Object(
            type = symbol.type.toAssemblyType(),
            signed = symbol.type.signed(),
            static = when (symbol.attributes) {
                VariableAttributes.Local -> false
                is VariableAttributes.Static -> true
            },
        )
    }
}

fun Type.Data.toAssemblyType(): Assembly.Type = when (this) {
    Type.Int -> Assembly.Type.LongWord
    Type.UInt -> Assembly.Type.LongWord
    Type.Long -> Assembly.Type.QuadWord
    Type.ULong -> Assembly.Type.QuadWord
    Type.Double -> Assembly.Type.QuadWord
}
