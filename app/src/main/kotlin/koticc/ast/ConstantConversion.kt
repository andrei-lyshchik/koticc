package koticc.ast

fun AST.Expression.Constant.convertTo(type: Type.Data): AST.Expression.Constant = copy(
    value = value.convertTo(type),
    type = type,
)

fun AST.Constant.convertTo(type: Type.Data): AST.Constant = when (type) {
    is Type.Int -> when (this) {
        is AST.IntConstant -> this
        is AST.LongConstant -> AST.IntConstant(value.toInt())
        is AST.UIntConstant -> AST.IntConstant(value.toInt())
        is AST.ULongConstant -> AST.IntConstant(value.toInt())
        is AST.DoubleConstant -> AST.IntConstant(value.toInt())
    }
    is Type.Long -> when (this) {
        is AST.LongConstant -> this
        is AST.IntConstant -> AST.LongConstant(value.toLong())
        is AST.UIntConstant -> AST.LongConstant(value.toLong())
        is AST.ULongConstant -> AST.LongConstant(value.toLong())
        is AST.DoubleConstant -> AST.LongConstant(value.toLong())
    }
    is Type.UInt -> when (this) {
        is AST.UIntConstant -> this
        is AST.IntConstant -> AST.UIntConstant(value.toUInt())
        is AST.LongConstant -> AST.UIntConstant(value.toUInt())
        is AST.ULongConstant -> AST.UIntConstant(value.toUInt())
        is AST.DoubleConstant -> AST.UIntConstant(value.toUInt())
    }
    is Type.ULong -> when (this) {
        is AST.ULongConstant -> this
        is AST.IntConstant -> AST.ULongConstant(value.toULong())
        is AST.LongConstant -> AST.ULongConstant(value.toULong())
        is AST.UIntConstant -> AST.ULongConstant(value.toULong())
        is AST.DoubleConstant -> AST.ULongConstant(value.toULong())
    }
    Type.Double -> when (this) {
        is AST.DoubleConstant -> this
        is AST.IntConstant -> AST.DoubleConstant(value.toDouble())
        is AST.LongConstant -> AST.DoubleConstant(value.toDouble())
        is AST.UIntConstant -> AST.DoubleConstant(value.toDouble())
        is AST.ULongConstant -> AST.DoubleConstant(value.toDouble())
    }
    is Type.Pointer -> when (this) {
        is AST.DoubleConstant -> AST.ULongConstant(value.toULong())
        is AST.IntConstant -> AST.ULongConstant(value.toULong())
        is AST.LongConstant -> AST.ULongConstant(value.toULong())
        is AST.UIntConstant -> AST.ULongConstant(value.toULong())
        is AST.ULongConstant -> this
    }
    is Type.Array -> TODO()
}
