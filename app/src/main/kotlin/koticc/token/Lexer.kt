@file:Suppress("ktlint:standard:filename")

package koticc.token

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
import arrow.core.raise.ensureNotNull
import koticc.CompilerError

data class LexerError(val message: String, val location: Location) : CompilerError {
    override fun message(): String = "lexer error, at ${location.toDisplayString()}: $message"
}

fun lexer(input: String): Either<LexerError, List<TokenWithLocation>> = either {
    val result = mutableListOf<TokenWithLocation>()
    input.lineSequence().withIndex().forEach { (lineIndex, lineContent) ->
        var current = 0
        val line = lineIndex + 1
        while (current < lineContent.length) {
            val char = lineContent[current]
            when {
                char.isDigit() || char == '.' -> {
                    val parsedToken = parseNumberToken(lineContent, current, line).bind()
                    result.add(parsedToken.value)
                    current = parsedToken.newCurrent
                }
                char == '(' -> result.add(TokenWithLocation(Token.OpenParen, Location(line, current + 1)))
                char == ')' -> result.add(TokenWithLocation(Token.CloseParen, Location(line, current + 1)))
                char == '{' -> result.add(TokenWithLocation(Token.OpenBrace, Location(line, current + 1)))
                char == '}' -> result.add(TokenWithLocation(Token.CloseBrace, Location(line, current + 1)))
                char == '[' -> result.add(TokenWithLocation(Token.OpenBracket, Location(line, current + 1)))
                char == ']' -> result.add(TokenWithLocation(Token.CloseBracket, Location(line, current + 1)))
                char == ';' -> result.add(TokenWithLocation(Token.Semicolon, Location(line, current + 1)))
                char == '~' -> result.add(TokenWithLocation(Token.Tilde, Location(line, current + 1)))
                char == '+' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '+' -> {
                            result.add(TokenWithLocation(Token.DoublePlus, Location(line, current + 1)))
                            current++
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.PlusEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Plus, Location(line, current + 1)))
                    }
                }
                char == '-' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '-' -> {
                            result.add(TokenWithLocation(Token.DoubleMinus, Location(line, current + 1)))
                            current++
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.MinusEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Minus, Location(line, current + 1)))
                    }
                }
                char == '*' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.AsteriskEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Asterisk, Location(line, current + 1)))
                    }
                }
                char == '/' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.SlashEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Slash, Location(line, current + 1)))
                    }
                }
                char == '%' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.PercentEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Percent, Location(line, current + 1)))
                    }
                }
                char == '&' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '&' -> {
                            result.add(TokenWithLocation(Token.DoubleAmpersand, Location(line, current + 1)))
                            current++
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.AmpersandEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Ampersand, Location(line, current + 1)))
                    }
                }
                char == '|' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '|' -> {
                            result.add(TokenWithLocation(Token.DoublePipe, Location(line, current + 1)))
                            current++
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.PipeEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Pipe, Location(line, current + 1)))
                    }
                }
                char == '^' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.CaretEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Caret, Location(line, current + 1)))
                    }
                }
                char == '<' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '<' -> {
                            if (lineContent.getOrNull(current + 2) == '=') {
                                result.add(
                                    TokenWithLocation(Token.DoubleLessThanEqual, Location(line, current + 1)),
                                )
                                current += 2
                            } else {
                                result.add(TokenWithLocation(Token.DoubleLessThan, Location(line, current + 1)))
                                current++
                            }
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.LessThanOrEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.LessThan, Location(line, current + 1)))
                    }
                }
                char == '>' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '>' -> {
                            if (lineContent.getOrNull(current + 2) == '=') {
                                result.add(
                                    TokenWithLocation(Token.DoubleGreaterThanEqual, Location(line, current + 1)),
                                )
                                current += 2
                            } else {
                                result.add(TokenWithLocation(Token.DoubleGreaterThan, Location(line, current + 1)))
                                current++
                            }
                        }
                        '=' -> {
                            result.add(TokenWithLocation(Token.GreaterThanOrEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.GreaterThan, Location(line, current + 1)))
                    }
                }
                char == '=' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.DoubleEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Equal, Location(line, current + 1)))
                    }
                }
                char == '!' -> {
                    when (lineContent.getOrNull(current + 1)) {
                        '=' -> {
                            result.add(TokenWithLocation(Token.ExclamationEqual, Location(line, current + 1)))
                            current++
                        }
                        else -> result.add(TokenWithLocation(Token.Exclamation, Location(line, current + 1)))
                    }
                }
                char == '?' -> result.add(TokenWithLocation(Token.QuestionMark, Location(line, current + 1)))
                char == ':' -> result.add(TokenWithLocation(Token.Colon, Location(line, current + 1)))
                char == ',' -> result.add(TokenWithLocation(Token.Comma, Location(line, current + 1)))
                char.isWhitespace() -> {}
                char.isIdentifierStart() -> {
                    val start = current
                    while (current + 1 < lineContent.length &&
                        (lineContent[current + 1].isIdentifierPart())
                    ) {
                        current++
                    }
                    val keywordToken = KEYWORD_TOKENS[lineContent.substring(start, current + 1)]
                    if (keywordToken != null) {
                        result.add(TokenWithLocation(keywordToken, Location(line, start + 1)))
                    } else {
                        result.add(
                            TokenWithLocation(
                                Token.Identifier(lineContent.substring(start, current + 1)),
                                Location(line, start + 1),
                            ),
                        )
                    }
                }
                else -> raise(LexerError("unexpected character: '$char'", Location(line, current + 1)))
            }
            current++
        }
    }

    result
}

private fun Char.isIdentifierStart() = isLetter() || this == '_'

private fun Char.isIdentifierPart() = isLetterOrDigit() || this == '_'

data class ParsedToken(
    val value: TokenWithLocation,
    val newCurrent: Int,
)

private val KEYWORD_TOKENS = mapOf(
    "int" to Token.IntKeyword,
    "long" to Token.LongKeyword,
    "void" to Token.Void,
    "return" to Token.Return,
    "if" to Token.If,
    "else" to Token.Else,
    "goto" to Token.Goto,
    "do" to Token.Do,
    "while" to Token.While,
    "for" to Token.For,
    "break" to Token.Break,
    "continue" to Token.Continue,
    "case" to Token.Case,
    "default" to Token.Default,
    "switch" to Token.Switch,
    "extern" to Token.Extern,
    "static" to Token.Static,
    "unsigned" to Token.UnsignedKeyword,
    "signed" to Token.SignedKeyword,
    "double" to Token.DoubleKeyword,
)

private val NUMBER_TOKENS = listOf(
    """([0-9]*\.[0-9]+|[0-9]+\.|(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+))([^\w.]|$)""".toRegex() to { match: MatchResult, line: Int, current: Int ->
        either {
            val doubleValue = match.groupValues[1].toDoubleOrNull()
            ensureNotNull(doubleValue) {
                LexerError("invalid double literal: '${match.value}'", Location(line, current + 1))
            }
            // The last character of the match is not part of the number literal
            ParsedToken(TokenWithLocation(Token.DoubleLiteral(doubleValue), Location(line, current + 1)), current + match.groupValues[1].length - 1)
        }
    },
    """(\d+)[lL]\b""".toRegex() to { match: MatchResult, line: Int, current: Int ->
        either {
            val longValue = match.groupValues[1].toLongOrNull()
            ensureNotNull(longValue) {
                LexerError("invalid long literal: '${match.value}'", Location(line, current + 1))
            }
            ParsedToken(TokenWithLocation(Token.LongLiteral(longValue), Location(line, current + 1)), current + match.value.length - 1)
        }
    },
    """(\d+)[uU]\b""".toRegex() to { match: MatchResult, line: Int, current: Int ->
        either {
            val unsignedIntValue = match.groupValues[1].toUIntOrNull()
            if (unsignedIntValue != null) {
                return@either ParsedToken(TokenWithLocation(Token.UIntLiteral(unsignedIntValue), Location(line, current + 1)), current + match.value.length - 1)
            }
            val unsignedLongValue = match.groupValues[1].toULongOrNull()
            ensureNotNull(unsignedLongValue) {
                LexerError("invalid unsigned number literal: '${match.value}'", Location(line, current + 1))
            }
            ParsedToken(TokenWithLocation(Token.ULongLiteral(unsignedLongValue), Location(line, current + 1)), current + match.value.length - 1)
        }
    },
    """(\d+)([uU][lL]|[lL][uU])\b""".toRegex() to { match: MatchResult, line: Int, current: Int ->
        either {
            val unsignedLongValue = match.groupValues[1].toULongOrNull()
            ensureNotNull(unsignedLongValue) {
                LexerError("invalid unsigned long literal: '${match.value}'", Location(line, current + 1))
            }
            ParsedToken(TokenWithLocation(Token.ULongLiteral(unsignedLongValue), Location(line, current + 1)), current + match.value.length - 1)
        }
    },
    """\d+\b""".toRegex() to { match: MatchResult, line: Int, current: Int ->
        either {
            val intValue = match.value.toIntOrNull()
            if (intValue != null) {
                return@either ParsedToken(TokenWithLocation(Token.IntLiteral(intValue), Location(line, current + 1)), current + match.value.length - 1)
            }
            val longValue = match.value.toLongOrNull()
            ensureNotNull(longValue) {
                LexerError("invalid number literal: '${match.value}'", Location(line, current + 1))
            }
            ParsedToken(TokenWithLocation(Token.LongLiteral(longValue), Location(line, current + 1)), current + match.value.length - 1)
        }
    },
)

private fun parseNumberToken(lineContent: String, current: Int, line: Int): Either<LexerError, ParsedToken> {
    for ((regex, parser) in NUMBER_TOKENS) {
        val match = regex.matchAt(lineContent, current)
        if (match != null) {
            return parser(match, line, current)
        }
    }

    val numberLiteral = lineContent.substring(current).takeWhile { it.isLetterOrDigit() }
    return LexerError("invalid number literal: '$numberLiteral'", Location(line, current + 1))
        .left()
}
