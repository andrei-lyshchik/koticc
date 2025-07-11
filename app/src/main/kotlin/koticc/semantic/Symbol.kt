package koticc.semantic

import koticc.ast.AST
import koticc.ast.Type

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
    data class Constant(val value: InitialConstantValue) : InitialValue
    data object NoInitializer : InitialValue
}

sealed interface InitialConstantValue {
    fun isZero(): Boolean

    data class Int(val value: kotlin.Int) : InitialConstantValue {
        override fun isZero(): Boolean = value == 0
    }
    data class UInt(val value: kotlin.UInt) : InitialConstantValue {
        override fun isZero(): Boolean = value == 0u
    }
    data class Long(val value: kotlin.Long) : InitialConstantValue {
        override fun isZero(): Boolean = value == 0L
    }
    data class ULong(val value: kotlin.ULong) : InitialConstantValue {
        override fun isZero(): Boolean = value == 0uL
    }
    data class Double(val value: kotlin.Double) : InitialConstantValue {
        override fun isZero(): Boolean = value == 0.0
    }
}

fun AST.Constant.toInitialValue(): InitialConstantValue = when (this) {
    is AST.IntConstant -> InitialConstantValue.Int(value)
    is AST.LongConstant -> InitialConstantValue.Long(value)
    is AST.UIntConstant -> InitialConstantValue.UInt(value)
    is AST.ULongConstant -> InitialConstantValue.ULong(value)
    is AST.DoubleConstant -> InitialConstantValue.Double(value)
}

fun Type.Data.toZeroInitialValue() = when (this) {
    is Type.Int -> InitialConstantValue.Int(0)
    is Type.UInt -> InitialConstantValue.UInt(0u)
    is Type.Long -> InitialConstantValue.Long(0)
    is Type.ULong -> InitialConstantValue.ULong(0uL)
    is Type.Double -> InitialConstantValue.Double(0.0)
    is Type.Pointer -> InitialConstantValue.ULong(0uL)
    is Type.Array -> TODO()
}

typealias SymbolTable = Map<String, Symbol>

fun SymbolTable.functionSymbol(functionName: String): Symbol.Function = this[functionName] as? Symbol.Function
    ?: error("Bug: function $functionName should have been added to the symbol table during semantic analysis, got $this")

fun SymbolTable.variableSymbol(variableName: String): Symbol.Variable = this[variableName]?.let { it as Symbol.Variable }
    ?: error("Bug: variable $variableName should have been added to the symbol table during semantic analysis, got $this")
