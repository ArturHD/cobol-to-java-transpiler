package de.netherspace.apps.actojat.languages.cobol

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor
import de.netherspace.apps.actojat.cobol_grammarParser
import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.languages.BaseVisitor
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode

class CobolVisitor : cobol_grammarBaseVisitor<JavaLanguageConstruct>(), BaseVisitor {

    private val methods = mutableMapOf<String, Method>()
    private val imports = mutableListOf<Import>()


    override fun visit(tree: ParseTree?): JavaLanguageConstruct {
        super.visit(tree) // TODO: pattern matching instead!
        return Program(
                methods = methods,
                imports = imports,
                comment = null
        )
    }

    override fun visitParagraph(ctx: cobol_grammarParser.ParagraphContext?): JavaLanguageConstruct {
        val sourceName: String = ctx
                ?.ID()
                ?.text
                ?: throw NullPointerException("Got a null value from the AST")
        val methodName: String = computeMethodName(ctx)

        val statements: List<Statement> = ctx
                .sentence()
                .map { sentenceToJavaStatement(it) }
                .toList()

        // TODO: COBOL statements have arguments as well (e.g. "PERFORM blockname counter TIMES")!
        val arguments: List<ArgumentDeclaration> = listOf()

        val javaMethod = Method(
                name = methodName,
                statements = statements,
                arguments = arguments,
                comment = null
        )
        methods[sourceName] = javaMethod
        return javaMethod // TODO: return a Pair<SourceName, JavaMethod> instead and collect them in a Stream!
    }

    override fun visitProceduredivision(ctx: cobol_grammarParser.ProceduredivisionContext?): JavaLanguageConstruct {
        // TODO: pattern matching!
        return super.visitChildren(ctx)
    }

    override fun visitImportcopyfile(ctx: cobol_grammarParser.ImportcopyfileContext?): JavaLanguageConstruct {
        val importName = computeImportName(
                ctx ?: throw NullPointerException("Got a null value from the AST")
        )
        val jimport = Import(
                name = importName,
                comment = null
        )
        imports.add(jimport) // TODO: the AST parent node should collect all imports from this function!
        return jimport
    }

    /**
     * Computes a Java method name from a COBOL paragraph.
     */
    private fun computeMethodName(paragraph: cobol_grammarParser.ParagraphContext?): String {
        val methodPrefix = "paragraph_" // TODO: Cobol sections might start with digits...
        val transformedSectiondecl : String = paragraph
                ?.ID()
                ?.text
                ?.replace("-", "_")
                ?: throw NullPointerException("Got a null value from the AST")
        return "$methodPrefix$transformedSectiondecl"
    }

    /**
     * Maps a COBOL sentence to a Java statement.
     */
    private fun sentenceToJavaStatement(sentence: cobol_grammarParser.SentenceContext): Statement {
        val cobolStatement = sentence.statement()[0]
        val statementType: CobolStatementType = when { // TODO: this should be done by the caller!
            cobolStatement.displayvalue() != null -> CobolStatementType.DISPLAY
            cobolStatement.performtimes() != null -> CobolStatementType.PERFORMTIMES
            cobolStatement.performuntil() != null -> CobolStatementType.PERFORMUNTIL
            cobolStatement.performvarying() != null -> CobolStatementType.PERFORMVARYING
            cobolStatement.performsinglefunction() != null -> CobolStatementType.PERFORMFUNCTION
            cobolStatement.stopoperation() != null -> CobolStatementType.STOPOPERATION
            else -> throw Exception("Unrecognized COBOL statement type!")
        }

        return when (statementType) {
            CobolStatementType.DISPLAY -> {
                // "DISPLAY ... ":
                val functionName = "DISPLAY" // TODO: make the enum hold these values!
                val parameters: List<Expression> = cobolStatement
                        .displayvalue()
                        .STRINGVALUE()
                        .map { stringvalueToJavaExpression(it) }
                        .toList()
                FunctionCall(
                        name = functionName,
                        parameters = parameters,
                        comment = null
                )
            }

            CobolStatementType.PERFORMTIMES -> {
                // "PERFORM ... TIMES":
                val performtimes = cobolStatement.performtimes()
                val cobolLoopCounter = performtimes.counter()
                val blockname = performtimes.blockname()

                val functionName = blockname.text
                val parameters = listOf<Expression>()
                val functionCall = FunctionCall(
                        name = functionName,
                        parameters = parameters,
                        comment = null
                )
                val loopCounter: String = computeForLoopCounter(cobolLoopCounter)

                val body = arrayOf(functionCall)
                val loopVariable: Assignment? = null // TODO: ...
                val loopCondition: String? = null // TODO: ...
                val loopIncrement: String? = null // TODO: ...
//                ForLoop(
//                        loopVariable = loopVariable,
//                        loopCondition = loopCondition,
//                        loopIncrement = loopIncrement,
//                        body = body,
//                        comment = null
//                )
                TODO("not implemented")
            }

            CobolStatementType.PERFORMUNTIL -> {
                // "PERFORM ... UNTIL":
                TODO("not implemented")
            }

            CobolStatementType.PERFORMVARYING -> {
                TODO("not implemented")
            }

            CobolStatementType.PERFORMFUNCTION -> {
                val functionName = cobolStatement.text
                val parameters = listOf<Expression>()
                FunctionCall(
                        name = functionName,
                        parameters = parameters,
                        comment = null
                )
            }

            CobolStatementType.STOPOPERATION -> {
                // "STOP ...":
                val functionName = "STOP"
                val parameters = listOf<Expression>()
                FunctionCall(
                        name = functionName,
                        parameters = parameters,
                        comment = null
                )
            }
        }
    }

    /**
     * Maps a Cobol import to a Java's import file name.
     */
    private fun computeImportName(ctx: cobol_grammarParser.ImportcopyfileContext): String {
        val importPrefix = "cobol_" // TODO: Cobol imports might start with digits...
        val transformedImport = ctx
                .FILEID()
                .text
                .replace(Regex("\\."), "_") // import without separating dot
                .replace("-", "_")
        return "$importPrefix$transformedImport"
    }

    /**
     * Maps a STRINGVALUE node to a Java Expression.
     */
    private fun <N : TerminalNode> stringvalueToJavaExpression(stringvalue: N): Expression {
        val parts = arrayOf(stringvalue.text)
        return Expression(
                parts = parts,
                comment = null
        )
    }

    /**
     * Computes a Java for-loop counter signature.
     */
    private fun computeForLoopCounter(cobolLoopCounter: cobol_grammarParser.CounterContext): String {
        val counter = cobolLoopCounter.text
        TODO("not implemented")
    }

    /**
     * An aux enum class: models different COBOL statement types.
     */
    enum class CobolStatementType {
        DISPLAY,
        PERFORMTIMES,
        PERFORMUNTIL,
        PERFORMVARYING,
        PERFORMFUNCTION,
        STOPOPERATION
    }
}
