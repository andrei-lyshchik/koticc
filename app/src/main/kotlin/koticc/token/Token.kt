package koticc.token

import koticc.common.Displayable

sealed interface Token : Displayable {
    data class Identifier(val value: String) : Token {
        override fun toDisplayString(): String = value
    }

    // literals
    data class IntLiteral(val value: Int) : Token {
        override fun toDisplayString(): String = value.toString()
    }

    data class LongLiteral(val value: Long) : Token {
        override fun toDisplayString(): String = value.toString()
    }

    data class UIntLiteral(val value: UInt) : Token {
        override fun toDisplayString(): String = value.toString() + "u"
    }

    data class ULongLiteral(val value: ULong) : Token {
        override fun toDisplayString(): String = value.toString() + "ul"
    }

    data class DoubleLiteral(val value: kotlin.Double) : Token {
        override fun toDisplayString(): String = value.toString()
    }

    // keywords
    data object Void : Token {
        override fun toDisplayString(): String = "void"
    }

    data object Return : Token {
        override fun toDisplayString(): String = "return"
    }

    data object If : Token {
        override fun toDisplayString(): String = "if"
    }

    data object Else : Token {
        override fun toDisplayString(): String = "else"
    }

    data object Goto : Token {
        override fun toDisplayString(): String = "goto"
    }

    data object Do : Token {
        override fun toDisplayString(): String = "do"
    }

    data object While : Token {
        override fun toDisplayString(): String = "while"
    }

    data object For : Token {
        override fun toDisplayString(): String = "for"
    }

    data object Break : Token {
        override fun toDisplayString(): String = "break"
    }

    data object Continue : Token {
        override fun toDisplayString(): String = "continue"
    }

    data object Case : Token {
        override fun toDisplayString(): String = "case"
    }

    data object Default : Token {
        override fun toDisplayString(): String = "default"
    }

    data object Switch : Token {
        override fun toDisplayString(): String = "switch"
    }

    data object Extern : Token {
        override fun toDisplayString(): String = "extern"
    }

    data object Static : Token {
        override fun toDisplayString(): String = "static"
    }

    // types
    data object IntKeyword : Token {
        override fun toDisplayString(): String = "int"
    }

    data object LongKeyword : Token {
        override fun toDisplayString(): String = "long"
    }

    data object SignedKeyword : Token {
        override fun toDisplayString(): String = "signed"
    }

    data object UnsignedKeyword : Token {
        override fun toDisplayString(): String = "unsigned"
    }

    data object DoubleKeyword : Token {
        override fun toDisplayString(): String = "double"
    }

    // punctuation
    data object OpenParen : Token {
        override fun toDisplayString(): String = "("
    }

    data object CloseParen : Token {
        override fun toDisplayString(): String = ")"
    }

    data object OpenBrace : Token {
        override fun toDisplayString(): String = "{"
    }

    data object CloseBrace : Token {
        override fun toDisplayString(): String = "}"
    }

    data object OpenBracket : Token {
        override fun toDisplayString(): String = "["
    }

    data object CloseBracket : Token {
        override fun toDisplayString(): String = "]"
    }

    data object Semicolon : Token {
        override fun toDisplayString(): String = ";"
    }

    data object Comma : Token {
        override fun toDisplayString(): String = ","
    }

    // assignment
    data object Equal : Token {
        override fun toDisplayString(): String = "="
    }

    // arithmetic operators
    data object Minus : Token {
        override fun toDisplayString(): String = "-"
    }

    data object Plus : Token {
        override fun toDisplayString(): String = "+"
    }

    data object Tilde : Token {
        override fun toDisplayString(): String = "~"
    }

    data object Asterisk : Token {
        override fun toDisplayString(): String = "*"
    }

    data object Slash : Token {
        override fun toDisplayString(): String = "/"
    }

    data object Percent : Token {
        override fun toDisplayString(): String = "%"
    }

    // bitwise operators
    data object Caret : Token {
        override fun toDisplayString(): String = "^"
    }

    data object Ampersand : Token {
        override fun toDisplayString(): String = "&"
    }

    data object Pipe : Token {
        override fun toDisplayString(): String = "|"
    }

    // shift operators
    data object DoubleLessThan : Token {
        override fun toDisplayString(): String = "<<"
    }

    data object DoubleGreaterThan : Token {
        override fun toDisplayString(): String = ">>"
    }

    // comparison operators
    data object LessThan : Token {
        override fun toDisplayString(): String = "<"
    }

    data object GreaterThan : Token {
        override fun toDisplayString(): String = ">"
    }

    data object DoubleEqual : Token {
        override fun toDisplayString(): String = "=="
    }

    data object LessThanOrEqual : Token {
        override fun toDisplayString(): String = "<="
    }

    data object GreaterThanOrEqual : Token {
        override fun toDisplayString(): String = ">="
    }

    data object ExclamationEqual : Token {
        override fun toDisplayString(): String = "!="
    }

    // logical operators
    data object DoubleAmpersand : Token {
        override fun toDisplayString(): String = "&&"
    }

    data object DoublePipe : Token {
        override fun toDisplayString(): String = "||"
    }

    // punctuation
    data object Exclamation : Token {
        override fun toDisplayString(): String = "!"
    }

    // compound assignment
    data object PlusEqual : Token {
        override fun toDisplayString(): String = "+="
    }

    data object MinusEqual : Token {
        override fun toDisplayString(): String = "-="
    }

    data object AsteriskEqual : Token {
        override fun toDisplayString(): String = "*="
    }

    data object SlashEqual : Token {
        override fun toDisplayString(): String = "/="
    }

    data object PercentEqual : Token {
        override fun toDisplayString(): String = "%="
    }

    // bitwise compound assignment
    data object AmpersandEqual : Token {
        override fun toDisplayString(): String = "&="
    }

    data object CaretEqual : Token {
        override fun toDisplayString(): String = "^="
    }

    data object PipeEqual : Token {
        override fun toDisplayString(): String = "|="
    }

    // shift compound assignment
    data object DoubleLessThanEqual : Token {
        override fun toDisplayString(): String = "<<="
    }

    data object DoubleGreaterThanEqual : Token {
        override fun toDisplayString(): String = ">>="
    }

    // increment and decrement
    data object DoublePlus : Token {
        override fun toDisplayString(): String = "++"
    }

    data object DoubleMinus : Token {
        override fun toDisplayString(): String = "--"
    }

    // conditional operator / goto label

    data object QuestionMark : Token {
        override fun toDisplayString(): String = "?"
    }

    data object Colon : Token {
        override fun toDisplayString(): String = ":"
    }
}

data class TokenWithLocation(
    val value: Token,
    val location: Location,
)

data class Location(
    val line: Int,
    val column: Int,
) : Displayable {
    override fun toDisplayString(): String = "line $line, column $column"
}

interface LocationAware {
    val location: Location
}
