package koticc.semantic

import koticc.ast.Type

fun Type.Function.toSymbol(defined: Boolean = true, global: Boolean = true): Symbol.Function =
    Symbol.Function(this, defined, global)

fun Type.Data.toSymbol(
    attributes: VariableAttributes = VariableAttributes.Local,
) = Symbol.Variable(this, attributes)

fun tempVariablesSymbolTable(start: Int, count: Int, type: Type.Data = Type.Int) = buildMap<String, Symbol> {
    repeat(count) { i -> put("tmp.${i + start}", type.toSymbol()) }
}
