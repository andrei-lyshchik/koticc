package koticc

sealed interface Token {
    data class Identifier(val value: String) : Token

    // literals
    data class IntLiteral(val value: Int) : Token

    // keywords
    data object Void : Token

    data object Return : Token

    data object If : Token

    data object Else : Token

    data object Goto : Token

    // types
    data object IntKeyword : Token

    // punctuation
    data object OpenParen : Token

    data object CloseParen : Token

    data object OpenBrace : Token

    data object CloseBrace : Token

    data object Semicolon : Token

    // assignment
    data object Equal : Token

    // arithmetic operators
    data object Minus : Token

    data object Plus : Token

    data object Tilde : Token

    data object Asterisk : Token

    data object Slash : Token

    data object Percent : Token

    // bitwise operators
    data object Caret : Token

    data object Ampersand : Token

    data object Pipe : Token

    // shift operators
    data object DoubleLessThan : Token

    data object DoubleGreaterThan : Token

    // comparison operators
    data object LessThan : Token

    data object GreaterThan : Token

    data object DoubleEqual : Token

    data object LessThanOrEqual : Token

    data object GreaterThanOrEqual : Token

    data object ExclamationEqual : Token

    // logical operators
    data object DoubleAmpersand : Token

    data object DoublePipe : Token

    data object Exclamation : Token

    // compound assignment
    data object PlusEqual : Token

    data object MinusEqual : Token

    data object AsteriskEqual : Token

    data object SlashEqual : Token

    data object PercentEqual : Token

    // bitwise compound assignment
    data object AmpersandEqual : Token

    data object CaretEqual : Token

    data object PipeEqual : Token

    // shift compound assignment
    data object DoubleLessThanEqual : Token

    data object DoubleGreaterThanEqual : Token

    // increment and decrement
    data object DoublePlus : Token

    data object DoubleMinus : Token

    // conditional operator / goto label

    data object QuestionMark : Token

    data object Colon : Token
}

data class TokenWithLocation(
    val value: Token,
    val location: Location,
)

data class Location(
    val line: Int,
    val column: Int,
) {
    fun toHumanReadableString(): String = "line $line, column $column"
}

interface LocationAware {
    val location: Location
}
