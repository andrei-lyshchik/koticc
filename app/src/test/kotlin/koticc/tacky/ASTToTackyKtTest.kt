package koticc.tacky

import koticc.VarargArgumentsProvider
import koticc.ast.AST
import koticc.ast.and
import koticc.ast.assign
import koticc.ast.cond
import koticc.ast.div
import koticc.ast.e
import koticc.ast.eq
import koticc.ast.ne
import koticc.ast.or
import koticc.ast.plus
import koticc.ast.program
import koticc.ast.times
import koticc.semantic.Type
import koticc.semantic.ValidASTProgram
import koticc.semantic.toIdentifier
import koticc.token.Location
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ArgumentsSource
import kotlin.test.assertEquals

class ASTToTackyKtTest {
    @Test
    fun `should produce tacky for a program with single return`() {
        val program =
            ValidASTProgram(
                value =
                program {
                    function("main") {
                        return_(1.e)
                    }
                },
                renamedVariableCount = 0,
                typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    return_(1.t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    class UnaryOperatorTestCases : VarargArgumentsProvider(
        AST.UnaryOperator.Negate to Tacky.UnaryOperator.Negate,
        AST.UnaryOperator.Complement to Tacky.UnaryOperator.Complement,
        AST.UnaryOperator.LogicalNegate to Tacky.UnaryOperator.LogicalNegate,
    )

    @ParameterizedTest
    @ArgumentsSource(UnaryOperatorTestCases::class)
    fun `should generate tacky for unary expression`(
        astUnaryOperator: AST.UnaryOperator,
        tackyUnaryOperator: Tacky.UnaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Statement(
                                        AST.Statement.Expression(
                                            AST.Expression.Unary(
                                                operator = astUnaryOperator,
                                                operand = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                location = Location(1, 0),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(0, 0),
                        ),
                    ),
                ),
                renamedVariableCount = 0,
                typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                topLevel =
                listOf(
                    Tacky.TopLevel.FunctionDefinition(
                        Tacky.FunctionDefinition(
                            name = "main",
                            parameters = emptyList(),
                            global = true,
                            body =
                            listOf(
                                Tacky.Instruction.Unary(
                                    operator = tackyUnaryOperator,
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                        ),
                    ),
                ),
            ),
            tacky,
        )
    }

    class NonShortCircuitingBinaryExpressionTestCases : VarargArgumentsProvider(
        AST.BinaryOperator.Add to Tacky.BinaryOperator.Add,
        AST.BinaryOperator.Subtract to Tacky.BinaryOperator.Subtract,
        AST.BinaryOperator.Multiply to Tacky.BinaryOperator.Multiply,
        AST.BinaryOperator.Divide to Tacky.BinaryOperator.Divide,
        AST.BinaryOperator.Modulo to Tacky.BinaryOperator.Modulo,
        AST.BinaryOperator.LessThan to Tacky.BinaryOperator.LessThan,
        AST.BinaryOperator.LessThanOrEqual to Tacky.BinaryOperator.LessThanOrEqual,
        AST.BinaryOperator.Equal to Tacky.BinaryOperator.Equal,
        AST.BinaryOperator.NotEqual to Tacky.BinaryOperator.NotEqual,
        AST.BinaryOperator.GreaterThan to Tacky.BinaryOperator.GreaterThan,
        AST.BinaryOperator.GreaterThanOrEqual to Tacky.BinaryOperator.GreaterThanOrEqual,
        AST.BinaryOperator.BitwiseAnd to Tacky.BinaryOperator.BitwiseAnd,
        AST.BinaryOperator.BitwiseOr to Tacky.BinaryOperator.BitwiseOr,
        AST.BinaryOperator.BitwiseXor to Tacky.BinaryOperator.BitwiseXor,
        AST.BinaryOperator.ShiftLeft to Tacky.BinaryOperator.ShiftLeft,
        AST.BinaryOperator.ShiftRight to Tacky.BinaryOperator.ShiftRight,
    )

    @ParameterizedTest
    @ArgumentsSource(NonShortCircuitingBinaryExpressionTestCases::class)
    fun `should generate tacky for non-short-circuiting binary expression`(
        astBinaryOperator: AST.BinaryOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Statement(
                                        AST.Statement.Expression(
                                            AST.Expression.Binary(
                                                operator = astBinaryOperator,
                                                left = AST.Expression.IntLiteral(1, Location(1, 0)),
                                                right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(0, 0),
                        ),
                    ),
                ),
                renamedVariableCount = 0,
                typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                topLevel =
                listOf(
                    Tacky.TopLevel.FunctionDefinition(
                        Tacky.FunctionDefinition(
                            name = "main",
                            parameters = emptyList(),
                            global = true,
                            body =
                            listOf(
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.IntConstant(1),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.0"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                        ),
                    ),
                ),
            ),
            tacky,
        )
    }

    @Test
    fun `should generate tacky for complex non-short-circuiting expression`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    return_(1.e * 2.e + 3.e / 4.e)
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("tmp.0", 1.t * 2.t)
                    assign("tmp.1", 3.t / 4.t)
                    assign("tmp.2", "tmp.0".t + "tmp.1".t)
                    return_("tmp.2".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should generate tacky for logical and`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    return_(
                        (1.e eq 1.e) and (1.e ne 3.e),
                    )
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("tmp.0", 1.t eq 1.t)
                    jumpIfZero("tmp.0".t, "and_false.0")
                    assign("tmp.1", 1.t neq 3.t)
                    jumpIfZero("tmp.1".t, "and_false.0")
                    assign("tmp.2", 1.t)
                    jump("and_end.1")
                    label("and_false.0")
                    assign("tmp.2", 0.t)
                    label("and_end.1")
                    return_("tmp.2".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should generate tacky for logical or`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    return_(
                        (1.e eq 1.e) or (1.e ne 3.e),
                    )
                }
            },
            renamedVariableCount = 0,
            typedIdentifiers = mapOf("main" to Type.Function(parameterCount = 0).toIdentifier()),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("tmp.0", 1.t eq 1.t)
                    jumpIfNotZero("tmp.0".t, "or_true.0")
                    assign("tmp.1", 1.t neq 3.t)
                    jumpIfNotZero("tmp.1".t, "or_true.0")
                    assign("tmp.2", 0.t)
                    jump("or_end.1")
                    label("or_true.0")
                    assign("tmp.2", 1.t)
                    label("or_end.1")
                    return_("tmp.2".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for declaration with initializer`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    int("x") assign 1.e
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should skip declaration without initializer`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    int("x")
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should start temp variable numbering from variable count`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    int("x") assign 1.e + 2.e
                }
            },
            renamedVariableCount = 123,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("tmp.123", 1.t + 2.t)
                    assign("x", "tmp.123".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for assignment`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    int("x")
                    assign("x", 1.e)
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    class CompoundAssignmentTestCases : VarargArgumentsProvider(
        AST.CompoundAssignmentOperator.Add to Tacky.BinaryOperator.Add,
        AST.CompoundAssignmentOperator.Subtract to Tacky.BinaryOperator.Subtract,
        AST.CompoundAssignmentOperator.Multiply to Tacky.BinaryOperator.Multiply,
        AST.CompoundAssignmentOperator.Divide to Tacky.BinaryOperator.Divide,
        AST.CompoundAssignmentOperator.Modulo to Tacky.BinaryOperator.Modulo,
        AST.CompoundAssignmentOperator.BitwiseAnd to Tacky.BinaryOperator.BitwiseAnd,
        AST.CompoundAssignmentOperator.BitwiseOr to Tacky.BinaryOperator.BitwiseOr,
        AST.CompoundAssignmentOperator.BitwiseXor to Tacky.BinaryOperator.BitwiseXor,
        AST.CompoundAssignmentOperator.ShiftLeft to Tacky.BinaryOperator.ShiftLeft,
        AST.CompoundAssignmentOperator.ShiftRight to Tacky.BinaryOperator.ShiftRight,
    )

    @ParameterizedTest
    @ArgumentsSource(CompoundAssignmentTestCases::class)
    fun `should produce tacky for compound assignment`(
        astCompoundAssignmentOperator: AST.CompoundAssignmentOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        AST.Declaration.Variable(
                                            name = "x",
                                            initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                            storageClass = null,
                                            location = Location(1, 0),
                                        ),
                                    ),
                                    AST.BlockItem.Statement(
                                        AST.Statement.Expression(
                                            AST.Expression.CompoundAssignment(
                                                operator = astCompoundAssignmentOperator,
                                                left = AST.Expression.Variable("x", Location(1, 0)),
                                                right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(0, 0),
                        ),
                    ),
                ),
                renamedVariableCount = 1,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "x" to Type.Integer.toIdentifier(),
                ),
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                topLevel =
                listOf(
                    Tacky.TopLevel.FunctionDefinition(
                        Tacky.FunctionDefinition(
                            name = "main",
                            parameters = emptyList(),
                            global = true,
                            body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.1"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("tmp.1"),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                        ),
                    ),
                ),
            ),
            tacky,
        )
    }

    class PostfixTestCases : VarargArgumentsProvider(
        AST.PostfixOperator.Increment to Tacky.BinaryOperator.Add,
        AST.PostfixOperator.Decrement to Tacky.BinaryOperator.Subtract,
    )

    @ParameterizedTest
    @ArgumentsSource(PostfixTestCases::class)
    fun `should produce tacky for postfix operator`(
        postfixOperator: AST.PostfixOperator,
        tackyBinaryOperator: Tacky.BinaryOperator,
    ) {
        val program =
            ValidASTProgram(
                value =
                AST.Program(
                    declarations =
                    listOf(
                        AST.Declaration.Function(
                            name = "main",
                            parameters = emptyList(),
                            body =
                            AST.Block(
                                blockItems =
                                listOf(
                                    AST.BlockItem.Declaration(
                                        AST.Declaration.Variable(
                                            name = "x",
                                            initializer = AST.Expression.IntLiteral(1, Location(1, 0)),
                                            storageClass = null,
                                            location = Location(1, 0),
                                        ),
                                    ),
                                    AST.BlockItem.Declaration(
                                        AST.Declaration.Variable(
                                            name = "y",
                                            initializer =
                                            AST.Expression.Binary(
                                                operator = AST.BinaryOperator.Add,
                                                left =
                                                AST.Expression.Postfix(
                                                    operator = postfixOperator,
                                                    operand = AST.Expression.Variable("x", Location(1, 0)),
                                                ),
                                                right = AST.Expression.IntLiteral(2, Location(1, 0)),
                                            ),
                                            storageClass = null,
                                            location = Location(1, 0),
                                        ),
                                    ),
                                    AST.BlockItem.Statement(
                                        AST.Statement.Return(
                                            expression =
                                            AST.Expression.Binary(
                                                operator = AST.BinaryOperator.Add,
                                                left = AST.Expression.Variable("x", Location(1, 0)),
                                                right = AST.Expression.Variable("y", Location(1, 0)),
                                            ),
                                            location = Location(1, 0),
                                        ),
                                    ),
                                ),
                            ),
                            storageClass = null,
                            location = Location(0, 0),
                        ),
                    ),
                ),
                renamedVariableCount = 2,
                typedIdentifiers = mapOf(
                    "main" to Type.Function(parameterCount = 0).toIdentifier(),
                    "x" to Type.Integer.toIdentifier(),
                    "y" to Type.Integer.toIdentifier(),
                ),
            )

        val tacky = programASTToTacky(program)

        assertEquals(
            Tacky.Program(
                topLevel =
                listOf(
                    Tacky.TopLevel.FunctionDefinition(
                        Tacky.FunctionDefinition(
                            name = "main",
                            parameters = emptyList(),
                            global = true,
                            body =
                            listOf(
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("x"),
                                    dst = Tacky.Value.Variable("tmp.2"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = tackyBinaryOperator,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.IntConstant(1),
                                    dst = Tacky.Value.Variable("x"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.Variable("tmp.2"),
                                    right = Tacky.Value.IntConstant(2),
                                    dst = Tacky.Value.Variable("tmp.3"),
                                ),
                                Tacky.Instruction.Copy(
                                    src = Tacky.Value.Variable("tmp.3"),
                                    dst = Tacky.Value.Variable("y"),
                                ),
                                Tacky.Instruction.Binary(
                                    operator = Tacky.BinaryOperator.Add,
                                    left = Tacky.Value.Variable("x"),
                                    right = Tacky.Value.Variable("y"),
                                    dst = Tacky.Value.Variable("tmp.4"),
                                ),
                                Tacky.Instruction.Return(Tacky.Value.Variable("tmp.4")),
                                Tacky.Instruction.Return(Tacky.Value.IntConstant(0)),
                            ),
                        ),
                    ),
                ),
            ),
            tacky,
        )
    }

    @Test
    fun `should produce tacky for if statement without else`() {
        val input = ValidASTProgram(
            value = program {
                function("main") {
                    int("x") assign 1.e
                    int("y") assign 0.e
                    if_("x".e eq 1.e) {
                        assign("y", 2.e)
                    }
                    return_("y".e)
                }
            },
            renamedVariableCount = 2,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
                "y" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(input)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    assign("y", 0.t)
                    assign("tmp.2", "x".t eq 1.t)
                    jumpIfZero("tmp.2".t, "if_else.0")
                    assign("y", 2.t)
                    label("if_else.0")
                    return_("y".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for if statement with else`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    int("x") assign 1.e
                    int("y") assign 0.e
                    if_("x".e eq 1.e) {
                        assign("y", 2.e)
                    } else_ {
                        assign("y", 3.e)
                    }
                    return_("y".e)
                }
            },
            renamedVariableCount = 2,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
                "y" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    assign("y", 0.t)
                    assign("tmp.2", "x".t eq 1.t)
                    jumpIfZero("tmp.2".t, "if_else.0")
                    assign("y", 2.t)
                    jump("if_end.1")
                    label("if_else.0")
                    assign("y", 3.t)
                    label("if_end.1")
                    return_("y".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for conditional expression`() {
        val program = ValidASTProgram(
            value =
            program {
                function("main") {
                    int("a.0") assign 1.e
                    int("b.1")
                    assign("a.0", cond("a.0".e eq 1.e, "b.1".e assign 2.e, "b.1".e assign 3.e))
                    return_("b.1".e)
                }
            },
            renamedVariableCount = 2,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "a.0" to Type.Integer.toIdentifier(),
                "b.1" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("a.0", 1.t)
                    assign("tmp.2", "a.0".t eq 1.t)
                    jumpIfZero("tmp.2".t, "cond_else.0")
                    assign("b.1", 2.t)
                    assign("tmp.3", "b.1".t)
                    jump("cond_end.1")
                    label("cond_else.0")
                    assign("b.1", 3.t)
                    assign("tmp.3", "b.1".t)
                    label("cond_end.1")
                    assign("a.0", "tmp.3".t)
                    return_("b.1".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for labeled statements and goto`() {
        val program = ValidASTProgram(
            value = program {
                function("main") {
                    int("x") assign 1.e
                    label("label") {
                        assign("x", 2.e)
                    }
                    goto("label")
                    return_(0.e)
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(program)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    label("label")
                    assign("x", 2.t)
                    jump("label")
                    return_(0.t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }

    @Test
    fun `should produce tacky for compound statements`() {
        val input = ValidASTProgram(
            value = program {
                function("main") {
                    nested {
                        int("x") assign 1.e
                        assign("x", 2.e)
                    }
                    return_("x".e)
                }
            },
            renamedVariableCount = 1,
            typedIdentifiers = mapOf(
                "main" to Type.Function(parameterCount = 0).toIdentifier(),
                "x" to Type.Integer.toIdentifier(),
            ),
        )

        val tacky = programASTToTacky(input)

        assertEquals(
            tackyProgram {
                function("main") {
                    assign("x", 1.t)
                    assign("x", 2.t)
                    return_("x".t)
                    return_(0.t)
                }
            },
            tacky,
        )
    }
}
