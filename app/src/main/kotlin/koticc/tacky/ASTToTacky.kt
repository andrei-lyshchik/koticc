package koticc.tacky

import koticc.ast.AST
import koticc.ast.LabelName
import koticc.ast.Type
import koticc.ast.convertTo
import koticc.semantic.InitialValue
import koticc.semantic.Symbol
import koticc.semantic.SymbolTable
import koticc.semantic.ValidASTProgram
import koticc.semantic.VariableAttributes
import koticc.semantic.functionSymbol
import koticc.semantic.toZeroInitialValue
import koticc.semantic.variableSymbol

fun programASTToTacky(validASTProgram: ValidASTProgram): Tacky.Program {
    val generator = TackyGenerator(
        initialVariableCount = validASTProgram.renamedVariableCount,
        symbolTable = validASTProgram.symbolTable,
    )
    return generator.generateProgram(validASTProgram.value)
}

private class TackyGenerator(initialVariableCount: Int, private val symbolTable: SymbolTable) {
    private val tempVariableSymbolTable: MutableMap<String, Symbol.Variable> = mutableMapOf()
    private var variableCount = initialVariableCount
    private val instructions: MutableList<Tacky.Instruction> = mutableListOf()
    private var labelCount = 0

    private fun nextVariable(type: Type.Data): Tacky.Value.Variable {
        val name = "tmp.${variableCount++}"
        tempVariableSymbolTable[name] = Symbol.Variable(type, VariableAttributes.Local)
        return Tacky.Value.Variable(name)
    }

    private fun nextLabelName(prefix: String): LabelName = LabelName("$prefix.${labelCount++}")

    fun generateProgram(program: AST.Program): Tacky.Program = Tacky.Program(
        topLevel =
        generateFunctionDefinitions(program) + generateStaticVariables(symbolTable),
        symbolTable = symbolTable + tempVariableSymbolTable,
    )

    private fun generateFunctionDefinitions(program: AST.Program) = program.declarations
        .mapNotNull { declaration ->
            when (declaration) {
                is AST.Declaration.Function -> {
                    generateFunctionDefinition(declaration)
                }
                is AST.Declaration.Variable -> null
            }
        }.map(Tacky.TopLevel::FunctionDefinition)

    private fun generateStaticVariables(symbolTable: SymbolTable) = symbolTable
        .mapNotNull { (name, symbol) ->
            when (symbol) {
                is Symbol.Variable -> {
                    when (symbol.attributes) {
                        is VariableAttributes.Static -> {
                            val initialValue = when (val initialValue = symbol.attributes.initialValue) {
                                is InitialValue.Constant -> initialValue.value
                                InitialValue.Tentative -> symbol.type.toZeroInitialValue()
                                InitialValue.NoInitializer -> return@mapNotNull null
                            }
                            Tacky.StaticVariable(name, global = symbol.attributes.global, initialValue = initialValue, type = symbol.type)
                        }
                        else -> null
                    }
                }
                else -> null
            }
        }
        .map(Tacky.TopLevel::StaticVariable)

    private fun generateFunctionDefinition(functionDeclaration: AST.Declaration.Function): Tacky.FunctionDefinition? {
        val body = functionDeclaration.body ?: return null
        body.blockItems.forEach { generateBlockItem(it) }
        instructions.add(
            Tacky.Instruction.Return(
                value = Tacky.Value.Constant(AST.IntConstant(0)),
            ),
        )
        val tackyFunction = Tacky.FunctionDefinition(
            name = functionDeclaration.name,
            parameters = functionDeclaration.parameters.map { it.name },
            global = symbolTable.functionSymbol(functionDeclaration.name).global,
            body = instructions.toList(),
        )
        instructions.clear()
        return tackyFunction
    }

    private fun generateBlockItem(blockItem: AST.BlockItem) {
        when (blockItem) {
            is AST.BlockItem.Declaration -> when (blockItem.declaration) {
                is AST.Declaration.Variable -> generateVariableDeclaration(blockItem.declaration)
                is AST.Declaration.Function -> {}
            }
            is AST.BlockItem.Statement -> generateStatement(blockItem.statement)
        }
    }

    private fun generateVariableDeclaration(declaration: AST.Declaration.Variable) {
        val variableType = symbolTable.variableSymbol(declaration.name)
        if (variableType.attributes is VariableAttributes.Static) return

        val initialValue = declaration.initializer?.let { generateExpressionAndConvert(it) }
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
            is AST.Statement.BreakLoop -> generateBreakLoop(statement)
            is AST.Statement.Continue -> generateContinue(statement)
            is AST.Statement.Switch -> generateSwitch(statement)
            is AST.Statement.Case -> generateCase(statement)
            is AST.Statement.Default -> generateDefault(statement)
            is AST.Statement.BreakSwitch -> generateBreakSwitch(statement)
            is AST.Statement.Break -> error("Do not expect to see a break statement here, only break loop or break switch")
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
                value = generateExpressionAndConvert(statement.expression),
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

    private fun generateBreakLoop(breakLoopStatement: AST.Statement.BreakLoop) {
        instructions.add(
            Tacky.Instruction.Jump(loopEndLabel(breakLoopStatement.loopId)),
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
        val condition = generateExpressionAndConvert(doWhile.condition)
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
        val condition = generateExpressionAndConvert(whileStatement.condition)
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
        val condition = forStatement.condition?.let { generateExpressionAndConvert(it) }
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
            is AST.ForInitializer.Declaration -> generateVariableDeclaration(forInitializer.declaration)
        }
    }

    private fun generateExpression(expression: AST.Expression): TackyExpressionResult = when (expression) {
        is AST.Expression.Constant -> Tacky.Value.Constant(expression.value).toPlainOperand()
        is AST.Expression.Variable -> Tacky.Value.Variable(expression.name).toPlainOperand()
        is AST.Expression.Unary -> {
            val src = generateExpressionAndConvert(expression.operand)
            val dst = nextVariable(expression.resolvedType())
            instructions.add(
                Tacky.Instruction.Unary(
                    operator = expression.operator.toTackyOperator(),
                    src = src,
                    dst = dst,
                ),
            )
            dst.toPlainOperand()
        }
        is AST.Expression.Binary -> {
            when (val tackyOperator = expression.operator.toTackyOperator()) {
                is TackyBinaryOperator.NonShortCircuiting -> {
                    val left = generateExpressionAndConvert(expression.left)
                    val right = generateExpressionAndConvert(expression.right)
                    val dst = nextVariable(expression.resolvedType())
                    instructions.add(
                        Tacky.Instruction.Binary(
                            operator = tackyOperator.operator,
                            left = left,
                            right = right,
                            dst = dst,
                        ),
                    )
                    dst.toPlainOperand()
                }
                TackyBinaryOperator.ShortCircuitingAnd -> {
                    val left = generateExpressionAndConvert(expression.left)
                    val falseLabel = nextLabelName("and_false")
                    instructions.add(
                        Tacky.Instruction.JumpIfZero(
                            src = left,
                            target = falseLabel,
                        ),
                    )
                    val right = generateExpressionAndConvert(expression.right)
                    instructions.add(
                        Tacky.Instruction.JumpIfZero(
                            src = right,
                            target = falseLabel,
                        ),
                    )
                    val dst = nextVariable(expression.resolvedType())
                    instructions.add(
                        Tacky.Instruction.Copy(
                            src = Tacky.Value.Constant(AST.IntConstant(1)),
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
                            src = Tacky.Value.Constant(AST.IntConstant(0)),
                            dst = dst,
                        ),
                    )
                    instructions.add(
                        Tacky.Instruction.Label(endLabel),
                    )
                    dst.toPlainOperand()
                }
                TackyBinaryOperator.ShortCircuitingOr -> {
                    val left = generateExpressionAndConvert(expression.left)
                    val trueLabel = nextLabelName("or_true")
                    instructions.add(
                        Tacky.Instruction.JumpIfNotZero(
                            src = left,
                            target = trueLabel,
                        ),
                    )
                    val right = generateExpressionAndConvert(expression.right)
                    instructions.add(
                        Tacky.Instruction.JumpIfNotZero(
                            src = right,
                            target = trueLabel,
                        ),
                    )
                    val dst = nextVariable(expression.resolvedType())
                    instructions.add(
                        Tacky.Instruction.Copy(
                            src = Tacky.Value.Constant(AST.IntConstant(0)),
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
                            src = Tacky.Value.Constant(AST.IntConstant(1)),
                            dst = dst,
                        ),
                    )
                    instructions.add(
                        Tacky.Instruction.Label(endLabel),
                    )
                    dst.toPlainOperand()
                }
            }
        }

        is AST.Expression.Assignment -> {
            val right = generateExpressionAndConvert(expression.right)
            when (val left = generateExpression(expression.left)) {
                is TackyExpressionResult.PlainOperand -> {
                    instructions.add(
                        Tacky.Instruction.Copy(
                            src = right,
                            dst = left.value,
                        ),
                    )
                    left
                }
                is TackyExpressionResult.DereferencedPointer -> {
                    instructions.add(
                        Tacky.Instruction.Store(
                            src = right,
                            dstPtr = left.ptr,
                        ),
                    )
                    right.toPlainOperand()
                }
            }
        }
        is AST.Expression.Postfix -> {
            val operand = generateExpression(expression.operand)
            val tempValue = nextVariable(expression.resolvedType())
            when (operand) {
                is TackyExpressionResult.PlainOperand -> {
                    instructions.add(
                        Tacky.Instruction.Copy(
                            src = operand.value,
                            dst = tempValue,
                        ),
                    )
                }
                is TackyExpressionResult.DereferencedPointer -> {
                    instructions.add(
                        Tacky.Instruction.Load(
                            srcPtr = operand.ptr,
                            dst = tempValue,
                        ),
                    )
                }
            }

            val constant = AST.IntConstant(1).convertTo(expression.resolvedType())
            val right = Tacky.Value.Constant(constant)
            when (operand) {
                is TackyExpressionResult.PlainOperand -> {
                    instructions.add(
                        Tacky.Instruction.Binary(
                            operator = expression.operator.toTackyOperator(),
                            left = operand.value,
                            right = right,
                            dst = operand.value,
                        ),
                    )
                }
                is TackyExpressionResult.DereferencedPointer -> {
                    val tempValue2 = nextVariable(expression.resolvedType())
                    instructions.add(
                        Tacky.Instruction.Binary(
                            operator = expression.operator.toTackyOperator(),
                            left = tempValue,
                            right = right,
                            dst = tempValue2,
                        ),
                    )
                    instructions.add(
                        Tacky.Instruction.Store(
                            src = tempValue2,
                            dstPtr = operand.ptr,
                        ),
                    )
                }
            }

            tempValue.toPlainOperand()
        }
        is AST.Expression.Conditional -> {
            val condition = generateExpressionAndConvert(expression.condition)
            val elseLabel = nextLabelName("cond_else")
            instructions.add(
                Tacky.Instruction.JumpIfZero(
                    src = condition,
                    target = elseLabel,
                ),
            )
            val thenValue = generateExpressionAndConvert(expression.thenExpression)
            val result = nextVariable(expression.resolvedType())
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
            val elseValue = generateExpressionAndConvert(expression.elseExpression)
            instructions.add(
                Tacky.Instruction.Copy(
                    src = elseValue,
                    dst = result,
                ),
            )
            instructions.add(
                Tacky.Instruction.Label(endLabel),
            )
            result.toPlainOperand()
        }
        is AST.Expression.FunctionCall -> {
            val arguments = expression.arguments.map { generateExpressionAndConvert(it) }
            val dst = nextVariable(expression.resolvedType())
            instructions.add(
                Tacky.Instruction.Call(
                    name = expression.name,
                    arguments = arguments,
                    dst = dst,
                ),
            )
            dst.toPlainOperand()
        }

        is AST.Expression.Cast -> {
            generateCast(expression).toPlainOperand()
        }

        is AST.Expression.AddressOf -> {
            when (val inner = generateExpression(expression.expression)) {
                is TackyExpressionResult.PlainOperand -> {
                    val dst = nextVariable(expression.resolvedType())
                    instructions.add(
                        Tacky.Instruction.GetAddress(inner.value, dst),
                    )
                    dst.toPlainOperand()
                }
                is TackyExpressionResult.DereferencedPointer -> {
                    inner.ptr.toPlainOperand()
                }
            }
        }
        is AST.Expression.Dereference -> {
            val inner = generateExpressionAndConvert(expression.expression)
            TackyExpressionResult.DereferencedPointer(inner)
        }
    }

    private fun generateExpressionAndConvert(expression: AST.Expression): Tacky.Value = when (val result = generateExpression(expression)) {
        is TackyExpressionResult.PlainOperand -> result.value
        is TackyExpressionResult.DereferencedPointer -> {
            val dst = nextVariable(expression.resolvedType())
            instructions.add(
                Tacky.Instruction.Load(result.ptr, dst),
            )
            dst
        }
    }

    private fun generateIf(ifStatement: AST.Statement.If) {
        val condition = generateExpressionAndConvert(ifStatement.condition)
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

    private fun generateSwitch(switch: AST.Statement.Switch) {
        val expression = generateExpressionAndConvert(switch.expression)
        requireNotNull(switch.switchId) {
            "switchId must be filled in during semantic analysis"
        }
        requireNotNull(switch.caseExpressions) {
            "caseExpressions must be filled in during semantic analysis"
        }

        switch.caseExpressions.forEach { (caseExpression, caseId) ->
            val caseLabel = caseLabel(switch.switchId, caseId)
            val caseValue = Tacky.Value.Constant(caseExpression)
            val diff = nextVariable(switch.expression.resolvedType())
            instructions.add(
                Tacky.Instruction.Binary(
                    operator = Tacky.BinaryOperator.Equal,
                    left = expression,
                    right = caseValue,
                    dst = diff,
                ),
            )
            instructions.add(
                Tacky.Instruction.JumpIfNotZero(
                    src = diff,
                    target = caseLabel,
                ),
            )
        }
        if (switch.hasDefault) {
            instructions.add(
                Tacky.Instruction.Jump(defaultLabel(switch.switchId)),
            )
        } else {
            instructions.add(
                Tacky.Instruction.Jump(switchEndLabel(switch.switchId)),
            )
        }

        generateStatement(switch.body)
        instructions.add(
            Tacky.Instruction.Label(switchEndLabel(switch.switchId)),
        )
    }

    private fun generateCase(case: AST.Statement.Case) {
        requireNotNull(case.switchId) {
            "switchId must be filled in during semantic analysis"
        }
        requireNotNull(case.caseId) {
            "caseId must be filled in during semantic analysis"
        }
        instructions.add(
            Tacky.Instruction.Label(caseLabel(case.switchId, case.caseId)),
        )
        generateStatement(case.body)
    }

    private fun generateDefault(default: AST.Statement.Default) {
        requireNotNull(default.switchId) {
            "switchId must be filled in during semantic analysis"
        }
        instructions.add(
            Tacky.Instruction.Label(defaultLabel(default.switchId)),
        )
        generateStatement(default.body)
    }

    private fun generateBreakSwitch(breakSwitch: AST.Statement.BreakSwitch) {
        instructions.add(
            Tacky.Instruction.Jump(switchEndLabel(breakSwitch.switchId)),
        )
    }

    private fun caseLabel(switchId: AST.SwitchId, caseId: AST.CaseId) = LabelName("switch.${switchId.value}.case.${caseId.value}")

    private fun defaultLabel(switchId: AST.SwitchId) = LabelName("switch.${switchId.value}.default")

    private fun switchEndLabel(switchId: AST.SwitchId) = LabelName("switch.${switchId.value}.end")

    private fun AST.UnaryOperator.toTackyOperator(): Tacky.UnaryOperator = when (this) {
        AST.UnaryOperator.Negate -> Tacky.UnaryOperator.Negate
        AST.UnaryOperator.Complement -> Tacky.UnaryOperator.Complement
        AST.UnaryOperator.LogicalNegate -> Tacky.UnaryOperator.LogicalNegate
    }

    private fun generateCast(expression: AST.Expression.Cast): Tacky.Value {
        val tackyExpression = generateExpressionAndConvert(expression.expression)
        val innerExpressionType = expression.expression.resolvedType()

        if (expression.targetType == innerExpressionType) {
            return tackyExpression
        }

        val dst = nextVariable(expression.resolvedType())
        when {
            expression.targetType is Type.Double -> {
                if (innerExpressionType.signed()) {
                    instructions.add(
                        Tacky.Instruction.IntToDouble(
                            src = tackyExpression,
                            dst = dst,
                        ),
                    )
                } else {
                    instructions.add(
                        Tacky.Instruction.UIntToDouble(
                            src = tackyExpression,
                            dst = dst,
                        ),
                    )
                }
            }
            innerExpressionType is Type.Double -> {
                if (expression.targetType.signed()) {
                    instructions.add(
                        Tacky.Instruction.DoubleToInt(
                            src = tackyExpression,
                            dst = dst,
                        ),
                    )
                } else {
                    instructions.add(
                        Tacky.Instruction.DoubleToUInt(
                            src = tackyExpression,
                            dst = dst,
                        ),
                    )
                }
            }
            expression.targetType.size() == innerExpressionType.size() -> instructions.add(
                Tacky.Instruction.Copy(
                    src = tackyExpression,
                    dst = dst,
                ),
            )
            expression.targetType.size() < innerExpressionType.size() -> instructions.add(
                Tacky.Instruction.Truncate(
                    src = tackyExpression,
                    dst = dst,
                ),
            )
            innerExpressionType.signed() -> instructions.add(
                Tacky.Instruction.SignExtend(
                    src = tackyExpression,
                    dst = dst,
                ),
            )
            else -> instructions.add(
                Tacky.Instruction.ZeroExtend(
                    src = tackyExpression,
                    dst = dst,
                ),
            )
        }

        return dst
    }

    private fun AST.BinaryOperator.toTackyOperator() = when (this) {
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

    private fun AST.PostfixOperator.toTackyOperator(): Tacky.BinaryOperator = when (this) {
        AST.PostfixOperator.Increment -> Tacky.BinaryOperator.Add
        AST.PostfixOperator.Decrement -> Tacky.BinaryOperator.Subtract
    }
}

private sealed interface TackyBinaryOperator {
    data class NonShortCircuiting(val operator: Tacky.BinaryOperator) : TackyBinaryOperator

    data object ShortCircuitingAnd : TackyBinaryOperator

    data object ShortCircuitingOr : TackyBinaryOperator
}

private sealed interface TackyExpressionResult {
    data class PlainOperand(val value: Tacky.Value) : TackyExpressionResult
    data class DereferencedPointer(val ptr: Tacky.Value) : TackyExpressionResult
}

private fun Tacky.Value.toPlainOperand() = TackyExpressionResult.PlainOperand(this)
