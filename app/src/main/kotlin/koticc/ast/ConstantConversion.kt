package koticc.ast

fun AST.Constant.convertTo(type: Type.Data): AST.Constant {
    return when (type) {
        is Type.Int -> when (this) {
            is AST.IntConstant -> this
            is AST.LongConstant -> AST.IntConstant(value.toInt())
        }
        is Type.Long -> when (this) {
            is AST.IntConstant -> AST.LongConstant(value.toLong())
            is AST.LongConstant -> this
        }
    }
}
