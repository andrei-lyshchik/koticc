package koticc.semantic

fun Type.Function.toIdentifier(defined: Boolean = true, global: Boolean = true): TypedIdentifier.Function =
    TypedIdentifier.Function(this, defined, global)

fun Type.Integer.toIdentifier(
    attributes: VariableAttributes = VariableAttributes.Local,
) = TypedIdentifier.Variable(this, attributes)
