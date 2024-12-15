package koticc.semantic

import koticc.ast.Type

fun Type.Function.toIdentifier(defined: Boolean = true, global: Boolean = true): Symbol.Function =
    Symbol.Function(this, defined, global)

fun Type.Integer.toIdentifier(
    attributes: VariableAttributes = VariableAttributes.Local,
) = Symbol.Variable(this, attributes)
