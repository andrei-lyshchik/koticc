package koticc.token

import arrow.core.left
import arrow.core.right
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

class LexerKtTest {
    @Test
    fun `should return correct tokens with location`() {
        val input =
            """
int main(void) {
  return 123;
}
   """
                .trimIndent()

        assertEquals(
            listOf(
                TokenWithLocation(Token.IntKeyword, Location(1, 1)),
                TokenWithLocation(Token.Identifier("main"), Location(1, 5)),
                TokenWithLocation(Token.OpenParen, Location(1, 9)),
                TokenWithLocation(Token.Void, Location(1, 10)),
                TokenWithLocation(Token.CloseParen, Location(1, 14)),
                TokenWithLocation(Token.OpenBrace, Location(1, 16)),
                TokenWithLocation(Token.Return, Location(2, 3)),
                TokenWithLocation(Token.IntLiteral(123), Location(2, 10)),
                TokenWithLocation(Token.Semicolon, Location(2, 13)),
                TokenWithLocation(Token.CloseBrace, Location(3, 1)),
            )
                .right(),
            lexer(input),
        )
    }

    @Test
    fun `test all tokens`() {
        val input =
            """
            abc cde_fgh 123 void return int (){};=-+~*/%^&| << >> < > <= >= != && || ! += -=
            *= /= %= &= ^= |= <<= >>= ++ -- if else ifnot elsenot ? : goto _id
            do while for break continue case default switch , extern static
            """.trimIndent()

        assertEquals(
            listOf(
                Token.Identifier("abc"),
                Token.Identifier("cde_fgh"),
                Token.IntLiteral(123),
                Token.Void,
                Token.Return,
                Token.IntKeyword,
                Token.OpenParen,
                Token.CloseParen,
                Token.OpenBrace,
                Token.CloseBrace,
                Token.Semicolon,
                Token.Equal,
                Token.Minus,
                Token.Plus,
                Token.Tilde,
                Token.Asterisk,
                Token.Slash,
                Token.Percent,
                Token.Caret,
                Token.Ampersand,
                Token.Pipe,
                Token.DoubleLessThan,
                Token.DoubleGreaterThan,
                Token.LessThan,
                Token.GreaterThan,
                Token.LessThanOrEqual,
                Token.GreaterThanOrEqual,
                Token.ExclamationEqual,
                Token.DoubleAmpersand,
                Token.DoublePipe,
                Token.Exclamation,
                Token.PlusEqual,
                Token.MinusEqual,
                Token.AsteriskEqual,
                Token.SlashEqual,
                Token.PercentEqual,
                Token.AmpersandEqual,
                Token.CaretEqual,
                Token.PipeEqual,
                Token.DoubleLessThanEqual,
                Token.DoubleGreaterThanEqual,
                Token.DoublePlus,
                Token.DoubleMinus,
                Token.If,
                Token.Else,
                Token.Identifier("ifnot"),
                Token.Identifier("elsenot"),
                Token.QuestionMark,
                Token.Colon,
                Token.Goto,
                Token.Identifier("_id"),
                Token.Do,
                Token.While,
                Token.For,
                Token.Break,
                Token.Continue,
                Token.Case,
                Token.Default,
                Token.Switch,
                Token.Comma,
                Token.Extern,
                Token.Static,
            ).right(),
            lexer(input).map { tokens -> tokens.map { it.value } },
        )
    }

    @Test
    fun `int literals`() {
        val input = "123 0 2147483647 ${Int.MAX_VALUE.toLong() + 1} ${Long.MAX_VALUE} 100l 200L"

        assertEquals(
            listOf(
                Token.IntLiteral(123),
                Token.IntLiteral(0),
                Token.IntLiteral(2147483647),
                Token.LongLiteral(Int.MAX_VALUE.toLong() + 1),
                Token.LongLiteral(Long.MAX_VALUE),
                Token.LongLiteral(100),
                Token.LongLiteral(200),
            )
                .right(),
            lexer(input).map { tokens -> tokens.map { it.value } },
        )
    }

    @Test
    fun `should return error if long literal is followed by a digit`() {
        val input = "100l0"

        assertEquals(
            LexerError("invalid number literal: '100l0'", Location(1, 1)).left(),
            lexer(input),
        )
    }

    @Test
    fun `should correctly handle non-word chars after numbers`() {
        val input = "123+"

        assertEquals(
            listOf(
                Token.IntLiteral(123),
                Token.Plus,
            )
                .right(),
            lexer(input).map { tokens -> tokens.map { it.value } },
        )
    }

    @Test
    fun `should return error for unexpected char`() {
        val input = "$"

        assertEquals(
            LexerError("unexpected character: '\$'", Location(1, 1)).left(),
            lexer(input),
        )
    }

    @Test
    fun `should return error if int literal is followed by a char`() {
        val input = "123a"

        assertEquals(
            LexerError("invalid number literal: '123a'", Location(1, 1)).left(),
            lexer(input),
        )
    }

    @Test
    fun `should return long if int literal is too big to fit in an int`() {
        val input = "2147483648"

        assertEquals(
            listOf(
                Token.LongLiteral(2147483648),
            ).right(),
            lexer(input).map { tokens -> tokens.map { it.value } },
        )
    }

    @Test
    fun `should return error if long literal is followed by a char`() {
        val input = "123Lx"

        assertEquals(
            LexerError("invalid number literal: '123Lx'", Location(1, 1)).left(),
            lexer(input),
        )
    }

    @Test
    fun `should return error if long literal is too big to fit into long`() {
        val input = "9223372036854775808"

        assertEquals(
            LexerError("invalid number literal: '9223372036854775808'", Location(1, 1)).left(),
            lexer(input),
        )
    }
}
