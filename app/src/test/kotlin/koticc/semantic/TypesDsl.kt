package koticc.semantic

import koticc.ast.Type

fun Type.Function.toSymbol(defined: Boolean = true, global: Boolean = true): Symbol.Function =
    Symbol.Function(this, defined, global)

fun Type.Int.toSymbol(
    attributes: VariableAttributes = VariableAttributes.Local,
) = Symbol.Variable(this, attributes)
