package koticc

fun programASTToTacky(validASTProgram: ValidASTProgram): Tacky.Program {
    val generator = TackyGenerator(initialVariableCount = validASTProgram.variableCount)
    return generator.generateProgram(validASTProgram.value)
}

private class TackyGenerator(initialVariableCount: Int) {
    private var variableCount = initialVariableCount
    private val instructions: MutableList<Tacky.Instruction> = mutableListOf()
    private var labelCount = 0

    private fun nextVariable(): Tacky.Value.Variable = Tacky.Value.Variable("tmp.${variableCount++}")

    private fun nextLabelName(prefix: String): LabelName = LabelName("$prefix.${labelCount++}")

    fun generateProgram(program: AST.Program): Tacky.Program {
        program.functionDefinition.body.blockItems.forEach { generateBlockItem(it) }
        instructions.add(
            Tacky.Instruction.Return(
                value = Tacky.Value.IntConstant(0),
            ),
        )
        return Tacky.Program(
            functionDefinition =
            Tacky.FunctionDefinition(
                name = program.functionDefinition.name,
                body = instructions,
            ),
        )
    }

    private fun generateBlockItem(blockItem: AST.BlockItem) {
        when (blockItem) {
            is AST.BlockItem.Declaration -> generateDeclaration(blockItem.declaration)
            is AST.BlockItem.Statement -> generateStatement(blockItem.statement)
        }
    }

    private fun generateDeclaration(declaration: AST.Declaration) {
        val initialValue = declaration.initializer?.let { generateExpression(it) }
        val variable = Tacky.Value.Variable(declaration.name)
        if (initialValue != null) {
            instructions.add(
                Tacky.Instruction.Copy(
                    src = initialValue,
                    dst = variable,
                ),
            )
        }
    }

    private fun generateStatement(statement: AST.Statement) {
        when (statement) {
            is AST.Statement.Return -> generateReturn(statement)
            is AST.Statement.Expression -> generateExpression(statement.expression)
            is AST.Statement.Null -> Unit
            is AST.Statement.If -> generateIf(statement)
            is AST.Statement.Labeled -> generateLabeledStatement(statement)
            is AST.Statement.Goto -> generateGoto(statement)
            is AST.Statement.Compound -> generateBlock(statement.block)
            is AST.Statement.DoWhile -> generateDoWhile(statement)
            is AST.Statement.While -> generateWhile(statement)
            is AST.Statement.For -> generateFor(statement)
            is AST.Statement.Break -> generateBreak(statement)
            is AST.Statement.Continue -> generateContinue(statement)
        }
    }

    private fun generateGoto(statement: AST.Statement.Goto) {
        instructions.add(
            Tacky.Instruction.Jump(statement.label),
        )
    }

    private fun generateLabeledStatement(statement: AST.Statement.Labeled) {
        instructions.add(
            Tacky.Instruction.Label(statement.label),
        )
        generateStatement(statement.statement)
    }

    private fun generateBlock(blockItem: AST.Block) {
        blockItem.blockItems.forEach { generateBlockItem(it) }
    }

    private fun generateReturn(statement: AST.Statement.Return) {
        instructions.add(
            Tacky.Instruction.Return(
                value = generateExpression(statement.expression),
            ),
        )
    }

    private fun loopStartLabel(loopId: AST.LoopId): LabelName = LabelName("loop_start.${loopId.value}")
    private fun loopEndLabel(loopId: AST.LoopId): LabelName = LabelName("loop_end.${loopId.value}")
    private fun loopContinueLabel(loopId: AST.LoopId): LabelName = LabelName("loop_continue.${loopId.value}")

    private fun generateContinue(continueStatement: AST.Statement.Continue) {
        val loopId = requireNotNull(continueStatement.loopId) {
            "loopId must be filled in during semantic analysis"
        }
        instructions.add(
            Tacky.Instruction.Jump(loopContinueLabel(loopId)),
        )
    }

    private fun generateBreak(breakStatement: AST.Statement.Break) {
        val loopId = requireNotNull(breakStatement.loopId) {
            "loopId must be filled in during semantic analysis"
        }
        instructions.add(
            Tacky.Instruction.Jump(loopEndLabel(loopId)),
        )
    }

    private fun generateDoWhile(doWhile: AST.Statement.DoWhile) {
        val loopId = requireNotNull(doWhile.loopId) {
            "loopId must be filled in during semantic analysis"
        }
        val startLabel = loopStartLabel(loopId)
        instructions.add(
            Tacky.Instruction.Label(startLabel),
        )
        generateStatement(doWhile.body)
        instructions.add(Tacky.Instruction.Label(loopContinueLabel(loopId)))
        val condition = generateExpression(doWhile.condition)
        instructions.add(
            Tacky.Instruction.JumpIfNotZero(
                src = condition,
                target = startLabel,
            ),
        )
        instructions.add(
            Tacky.Instruction.Label(loopEndLabel(loopId)),
        )
    }

    private fun generateWhile(whileStatement: AST.Statement.While) {
        val loopId = requireNotNull(whileStatement.loopId) {
            "loopId must be filled in during semantic analysis"
        }
        val startLabel = loopStartLabel(loopId)
        instructions.add(
            Tacky.Instruction.Label(startLabel),
        )
        val condition = generateExpression(whileStatement.condition)
        instructions.add(
            Tacky.Instruction.JumpIfZero(
                src = condition,
                target = loopEndLabel(loopId),
            ),
        )
        generateStatement(whileStatement.body)
        instructions.add(
            Tacky.Instruction.Label(loopContinueLabel(loopId)),
        )
        instructions.add(
            Tacky.Instruction.Jump(startLabel),
        )
        instructions.add(
            Tacky.Instruction.Label(loopEndLabel(loopId)),
        )
    }

    private fun generateFor(forStatement: AST.Statement.For) {
        val loopId = requireNotNull(forStatement.loopId) {
            "loopId must be filled in during semantic analysis"
        }
        forStatement.initializer?.let { generateForInitializer(it) }
        val startLabel = loopStartLabel(loopId)
        instructions.add(
            Tacky.Instruction.Label(startLabel),
        )
        val condition = forStatement.condition?.let { generateExpression(it) }
        if (condition != null) {
            instructions.add(
                Tacky.Instruction.JumpIfZero(
                    src = condition,
                    target = loopEndLabel(loopId),
                ),
            )
        }
        generateStatement(forStatement.body)
        instructions.add(
            Tacky.Instruction.Label(loopContinueLabel(loopId)),
        )
        forStatement.post?.let { generateExpression(it) }
        instructions.add(
            Tacky.Instruction.Jump(startLabel),
        )
        instructions.add(
            Tacky.Instruction.Label(loopEndLabel(loopId)),
        )
    }

    private fun generateForInitializer(forInitializer: AST.ForInitializer) {
        when (forInitializer) {
            is AST.ForInitializer.Expression -> generateExpression(forInitializer.expression)
            is AST.ForInitializer.Declaration -> generateDeclaration(forInitializer.declaration)
        }
    }

    private fun generateExpression(expression: AST.Expression): Tacky.Value =
        when (expression) {
            is AST.Expression.IntLiteral -> Tacky.Value.IntConstant(expression.value)
            is AST.Expression.Variable -> Tacky.Value.Variable(expression.name)
            is AST.Expression.Unary -> {
                val src = generateExpression(expression.operand)
                val dst = nextVariable()
                instructions.add(
                    Tacky.Instruction.Unary(
                        operator = expression.operator.toTackyOperator(),
                        src = src,
                        dst = dst,
                    ),
                )
                dst
            }
            is AST.Expression.Binary -> {
                when (val tackyOperator = expression.operator.toTackyOperator()) {
                    is TackyBinaryOperator.NonShortCircuiting -> {
                        val left = generateExpression(expression.left)
                        val right = generateExpression(expression.right)
                        val dst = nextVariable()
                        instructions.add(
                            Tacky.Instruction.Binary(
                                operator = tackyOperator.operator,
                                left = left,
                                right = right,
                                dst = dst,
                            ),
                        )
                        dst
                    }
                    TackyBinaryOperator.ShortCircuitingAnd -> {
                        val left = generateExpression(expression.left)
                        val falseLabel = nextLabelName("and_false")
                        instructions.add(
                            Tacky.Instruction.JumpIfZero(
                                src = left,
                                target = falseLabel,
                            ),
                        )
                        val right = generateExpression(expression.right)
                        instructions.add(
                            Tacky.Instruction.JumpIfZero(
                                src = right,
                                target = falseLabel,
                            ),
                        )
                        val dst = nextVariable()
                        instructions.add(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.IntConstant(1),
                                dst = dst,
                            ),
                        )
                        val endLabel = nextLabelName("and_end")
                        instructions.add(
                            Tacky.Instruction.Jump(endLabel),
                        )
                        instructions.add(
                            Tacky.Instruction.Label(falseLabel),
                        )
                        instructions.add(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.IntConstant(0),
                                dst = dst,
                            ),
                        )
                        instructions.add(
                            Tacky.Instruction.Label(endLabel),
                        )
                        dst
                    }
                    TackyBinaryOperator.ShortCircuitingOr -> {
                        val left = generateExpression(expression.left)
                        val trueLabel = nextLabelName("or_true")
                        instructions.add(
                            Tacky.Instruction.JumpIfNotZero(
                                src = left,
                                target = trueLabel,
                            ),
                        )
                        val right = generateExpression(expression.right)
                        instructions.add(
                            Tacky.Instruction.JumpIfNotZero(
                                src = right,
                                target = trueLabel,
                            ),
                        )
                        val dst = nextVariable()
                        instructions.add(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.IntConstant(0),
                                dst = dst,
                            ),
                        )
                        val endLabel = nextLabelName("or_end")
                        instructions.add(
                            Tacky.Instruction.Jump(endLabel),
                        )
                        instructions.add(
                            Tacky.Instruction.Label(trueLabel),
                        )
                        instructions.add(
                            Tacky.Instruction.Copy(
                                src = Tacky.Value.IntConstant(1),
                                dst = dst,
                            ),
                        )
                        instructions.add(
                            Tacky.Instruction.Label(endLabel),
                        )
                        dst
                    }
                }
            }

            is AST.Expression.Assignment -> {
                val right = generateExpression(expression.right)
                val dst = generateExpression(expression.left)
                instructions.add(
                    Tacky.Instruction.Copy(
                        src = right,
                        dst = dst,
                    ),
                )
                dst
            }
            is AST.Expression.CompoundAssignment -> {
                val right = generateExpression(expression.right)
                val left = generateExpression(expression.left)
                val dst = nextVariable()
                instructions.add(
                    Tacky.Instruction.Binary(
                        operator = expression.operator.toTackyOperator(),
                        left = left,
                        right = right,
                        dst = dst,
                    ),
                )
                instructions.add(
                    Tacky.Instruction.Copy(
                        src = dst,
                        dst = left,
                    ),
                )
                dst
            }
            is AST.Expression.Postfix -> {
                val operand = generateExpression(expression.operand)
                val tempValue = nextVariable()
                instructions.add(
                    Tacky.Instruction.Copy(
                        src = operand,
                        dst = tempValue,
                    ),
                )
                instructions.add(
                    Tacky.Instruction.Binary(
                        operator = expression.operator.toTackyOperator(),
                        left = operand,
                        right = Tacky.Value.IntConstant(1),
                        dst = operand,
                    ),
                )

                tempValue
            }
            is AST.Expression.Conditional -> {
                val condition = generateExpression(expression.condition)
                val elseLabel = nextLabelName("cond_else")
                instructions.add(
                    Tacky.Instruction.JumpIfZero(
                        src = condition,
                        target = elseLabel,
                    ),
                )
                val thenValue = generateExpression(expression.thenExpression)
                val result = nextVariable()
                instructions.add(
                    Tacky.Instruction.Copy(
                        src = thenValue,
                        dst = result,
                    ),
                )
                val endLabel = nextLabelName("cond_end")
                instructions.add(
                    Tacky.Instruction.Jump(endLabel),
                )
                instructions.add(
                    Tacky.Instruction.Label(elseLabel),
                )
                val elseValue = generateExpression(expression.elseExpression)
                instructions.add(
                    Tacky.Instruction.Copy(
                        src = elseValue,
                        dst = result,
                    ),
                )
                instructions.add(
                    Tacky.Instruction.Label(endLabel),
                )
                result
            }
        }

    private fun generateIf(ifStatement: AST.Statement.If) {
        val condition = generateExpression(ifStatement.condition)
        val elseLabel = nextLabelName("if_else")
        instructions.add(
            Tacky.Instruction.JumpIfZero(
                src = condition,
                target = elseLabel,
            ),
        )
        generateStatement(ifStatement.thenStatement)
        ifStatement.elseStatement?.let { elseStatement ->
            val endLabel = nextLabelName("if_end")
            instructions.add(
                Tacky.Instruction.Jump(endLabel),
            )
            instructions.add(
                Tacky.Instruction.Label(elseLabel),
            )
            generateStatement(elseStatement)
            instructions.add(
                Tacky.Instruction.Label(endLabel),
            )
        } ?: instructions.add(
            Tacky.Instruction.Label(elseLabel),
        )
    }

    private fun AST.UnaryOperator.toTackyOperator(): Tacky.UnaryOperator =
        when (this) {
            AST.UnaryOperator.Negate -> Tacky.UnaryOperator.Negate
            AST.UnaryOperator.Complement -> Tacky.UnaryOperator.Complement
            AST.UnaryOperator.LogicalNegate -> Tacky.UnaryOperator.LogicalNegate
        }

    private fun AST.BinaryOperator.toTackyOperator() =
        when (this) {
            AST.BinaryOperator.Add -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Add)
            AST.BinaryOperator.Subtract -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Subtract)
            AST.BinaryOperator.Multiply -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Multiply)
            AST.BinaryOperator.Divide -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Divide)
            AST.BinaryOperator.Modulo -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Modulo)
            AST.BinaryOperator.Equal -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.Equal)
            AST.BinaryOperator.NotEqual -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.NotEqual)
            AST.BinaryOperator.LessThan -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.LessThan)
            AST.BinaryOperator.LessThanOrEqual -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.LessThanOrEqual)
            AST.BinaryOperator.GreaterThan -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.GreaterThan)
            AST.BinaryOperator.GreaterThanOrEqual -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.GreaterThanOrEqual)
            AST.BinaryOperator.BitwiseAnd -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.BitwiseAnd)
            AST.BinaryOperator.BitwiseOr -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.BitwiseOr)
            AST.BinaryOperator.BitwiseXor -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.BitwiseXor)
            AST.BinaryOperator.ShiftLeft -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.ShiftLeft)
            AST.BinaryOperator.ShiftRight -> TackyBinaryOperator.NonShortCircuiting(Tacky.BinaryOperator.ShiftRight)

            AST.BinaryOperator.LogicalAnd -> TackyBinaryOperator.ShortCircuitingAnd
            AST.BinaryOperator.LogicalOr -> TackyBinaryOperator.ShortCircuitingOr
        }

    private fun AST.CompoundAssignmentOperator.toTackyOperator(): Tacky.BinaryOperator =
        when (this) {
            AST.CompoundAssignmentOperator.Add -> Tacky.BinaryOperator.Add
            AST.CompoundAssignmentOperator.Subtract -> Tacky.BinaryOperator.Subtract
            AST.CompoundAssignmentOperator.Multiply -> Tacky.BinaryOperator.Multiply
            AST.CompoundAssignmentOperator.Divide -> Tacky.BinaryOperator.Divide
            AST.CompoundAssignmentOperator.Modulo -> Tacky.BinaryOperator.Modulo
            AST.CompoundAssignmentOperator.BitwiseAnd -> Tacky.BinaryOperator.BitwiseAnd
            AST.CompoundAssignmentOperator.BitwiseOr -> Tacky.BinaryOperator.BitwiseOr
            AST.CompoundAssignmentOperator.BitwiseXor -> Tacky.BinaryOperator.BitwiseXor
            AST.CompoundAssignmentOperator.ShiftLeft -> Tacky.BinaryOperator.ShiftLeft
            AST.CompoundAssignmentOperator.ShiftRight -> Tacky.BinaryOperator.ShiftRight
        }

    private fun AST.PostfixOperator.toTackyOperator(): Tacky.BinaryOperator =
        when (this) {
            AST.PostfixOperator.Increment -> Tacky.BinaryOperator.Add
            AST.PostfixOperator.Decrement -> Tacky.BinaryOperator.Subtract
        }
}

private sealed interface TackyBinaryOperator {
    data class NonShortCircuiting(val operator: Tacky.BinaryOperator) : TackyBinaryOperator

    data object ShortCircuitingAnd : TackyBinaryOperator

    data object ShortCircuitingOr : TackyBinaryOperator
}
