package koticc.semantic

import arrow.core.Either
import arrow.core.raise.either
import koticc.CompilerError
import koticc.ast.AST
import koticc.token.Location

data class ValidASTProgram(
    val value: AST.Program,
    val variableCount: Int,
    val types: Map<String, Type>,
)

data class SemanticAnalysisError(
    val message: String,
    val location: Location,
) : CompilerError {
    override fun message(): String = "semantic analysis error, at ${location.toHumanReadableString()}: $message"
}

fun semanticAnalysis(program: AST.Program): Either<SemanticAnalysisError, ValidASTProgram> =
    either {
        val identifierResolverResult = IdentifierResolver().resolveProgram(program).bind()
        val gotoLabelResolverResult = GotoLabelResolver().resolveLabels(identifierResolverResult.program).bind()
        val programWithResolvedLoopsAndSwitches = LoopAndSwitchResolver().resolveLoopsAndSwitches(gotoLabelResolverResult).bind()
        val types = Typechecker(identifierResolverResult.nameMapping).typecheck(programWithResolvedLoopsAndSwitches).bind()
        ValidASTProgram(
            value = programWithResolvedLoopsAndSwitches,
            variableCount = identifierResolverResult.variableCount,
            types = types,
        )
    }
