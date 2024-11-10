package koticc

import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure

data class LexerError(val message: String, val location: Location) : CompilerError {
    override fun message(): String = "lexer error, at ${location.toHumanReadableString()}: $message"
}

fun lexer(input: String): Either<CompilerError, List<TokenWithLocation>> =
    either {
        val result = mutableListOf<TokenWithLocation>()
        input.lineSequence().withIndex().forEach { (lineIndex, lineContent) ->
            var current = 0
            val line = lineIndex + 1
            while (current < lineContent.length) {
                val char = lineContent[current]
                when {
                    char.isDigit() -> {
                        val start = current
                        while (current + 1 < lineContent.length && lineContent[current + 1].isDigit()) {
                            current++
                        }
                        ensure(lineContent.getOrNull(current + 1)?.isLetter() != true) {
                            LexerError(
                                "unexpected character '${lineContent.getOrNull(current + 1)}' after int literal",
                                Location(line, current + 2),
                            )
                        }
                        val intLiteralString = lineContent.substring(start, current + 1)
                        val intLiteralValue =
                            intLiteralString
                                .parseInt()
                                .mapLeft {
                                    LexerError(
                                        "invalid integer literal: '$intLiteralString'",
                                        Location(line, start + 1),
                                    )
                                }
                                .bind()
                        result.add(
                            TokenWithLocation(Token.IntLiteral(intLiteralValue), Location(line, start + 1)),
                        )
                    }
                    char == '(' -> result.add(TokenWithLocation(Token.OpenParen, Location(line, current + 1)))
                    char == ')' -> result.add(TokenWithLocation(Token.CloseParen, Location(line, current + 1)))
                    char == '{' -> result.add(TokenWithLocation(Token.OpenBrace, Location(line, current + 1)))
                    char == '}' -> result.add(TokenWithLocation(Token.CloseBrace, Location(line, current + 1)))
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
                    char.isWhitespace() -> {}
                    char.isIdentifierStart() -> {
                        val start = current
                        while (current + 1 < lineContent.length &&
                            (lineContent[current + 1].isIdentifierPart())
                        ) {
                            current++
                        }
                        when (val identifier = lineContent.substring(start, current + 1)) {
                            "int" -> result.add(TokenWithLocation(Token.IntKeyword, Location(line, start + 1)))
                            "void" -> result.add(TokenWithLocation(Token.Void, Location(line, start + 1)))
                            "return" -> result.add(TokenWithLocation(Token.Return, Location(line, start + 1)))
                            "if" -> result.add(TokenWithLocation(Token.If, Location(line, start + 1)))
                            "else" -> result.add(TokenWithLocation(Token.Else, Location(line, start + 1)))
                            "goto" -> result.add(TokenWithLocation(Token.Goto, Location(line, start + 1)))
                            else ->
                                result.add(
                                    TokenWithLocation(Token.Identifier(identifier), Location(line, start + 1)),
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

private fun String.parseInt() = Either.catchOrThrow<NumberFormatException, Int> { toInt() }
