package de.netherspace.apps.actojat.languages.cobol

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor
import de.netherspace.apps.actojat.cobol_grammarParser
import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.languages.BaseVisitor
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode
import java.util.concurrent.atomic.AtomicInteger

class CobolVisitor : cobol_grammarBaseVisitor<JavaLanguageConstruct>(), BaseVisitor {

    private val methods = mutableMapOf<String, Method>()
    private val imports = mutableListOf<Import>()
    private val fields = mutableMapOf<String, Field>()
    private val knownIDs = mutableListOf<String>()
    private val internalIdCounter = AtomicInteger(0)


    override fun visit(tree: ParseTree?): JavaLanguageConstruct {
        super.visit(tree) // TODO: pattern matching instead!
        return Program(
                methods = methods,
                imports = imports,
                fields = fields,
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
                .map { sentenceToJavaStatements(it) }
                .fold(listOf(), { acc, list -> acc + list })

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

    override fun visitDatadeclaration(ctx: cobol_grammarParser.DatadeclarationContext?): JavaLanguageConstruct {
        val fieldName: String = ctx?.ID()?.text
                ?: throw NullPointerException("Got a null value from the AST")
        // TODO: transform name? (It might not have a valid Java name...)

        val type: PrimitiveType = cobolPicToJavaType(ctx.datatype())
        val initvalue: String = ctx.datatype().initialvalue().text ?: ""

        val vardecl = VariableDeclaration.DeclarationWithInit(
                lhs = LeftHandSide(Type.BasicType(type), fieldName),
                rhs = initvalue,
                comment = null
        )
        val field = Field(
                modifier = "public", // TODO: should be an enum!
                declaration = vardecl,
                comment = null
        )

        fields[fieldName] = field
        return field
    }

    /**
     * Maps a COBOL ("PIC") type to a proper Java type annotation.
     */
    private fun cobolPicToJavaType(pic: cobol_grammarParser.DatatypeContext?): PrimitiveType {
        val size: String = pic?.size()?.text
                ?: throw NullPointerException("Got a null value from the AST")

        val cobolTypesToJavaMap = mapOf(
                "1" to PrimitiveType.SHORT,
                "2" to PrimitiveType.SHORT,
                "3" to PrimitiveType.SHORT,
                "4" to PrimitiveType.SHORT,
                "5" to PrimitiveType.INT,
                "6" to PrimitiveType.INT,
                "7" to PrimitiveType.INT,
                "8" to PrimitiveType.INT,
                "9" to PrimitiveType.INT,
                "10" to PrimitiveType.BIGINT
                // TODO: ...
        )
        return cobolTypesToJavaMap[size]
                ?: throw NoSuchElementException("Couldn't map COBOL type to a Java type!")
    }

    /**
     * Computes a Java method name from a COBOL paragraph.
     */
    private fun computeMethodName(paragraph: cobol_grammarParser.ParagraphContext?): String {
        val methodPrefix = "paragraph_" // TODO: Cobol sections might start with digits...
        val transformedSectiondecl: String = paragraph
                ?.ID()
                ?.text
                ?.replace("-", "_")
                ?: throw NullPointerException("Got a null value from the AST")
        return "$methodPrefix$transformedSectiondecl"
    }

    /**
     * Maps a COBOL import to a Java import file name.
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
     * Maps a COBOL sentence to a list of Java statements.
     */
    private fun sentenceToJavaStatements(sentence: cobol_grammarParser.SentenceContext): List<Statement> {
        return cobolStatementsToJavaStatements(sentence.statements())
    }

    /**
     * Computes a list of Java statements for a given list of COBOL StatementsContexts.
     */
    private fun cobolStatementsToJavaStatements(statements: cobol_grammarParser.StatementsContext): List<Statement> {
        return statements
                .statement()
                .map { Pair(it, computeStatementType(it)) }
                .map { cobolStatementToJavaStatement(it.first, it.second) }
                .toList()
    }

    /**
     * Figures out which CobolStatementType matches a given COBOL StatementContext.
     */
    private fun computeStatementType(cobolStatement: cobol_grammarParser.StatementContext): CobolStatementType {
        return when {
            cobolStatement.displayvalue() != null -> CobolStatementType.DISPLAY
            cobolStatement.performtimes() != null -> CobolStatementType.PERFORMTIMES
            cobolStatement.performuntil() != null -> CobolStatementType.PERFORMUNTIL
            cobolStatement.performvarying() != null -> CobolStatementType.PERFORMVARYING
            cobolStatement.performsinglefunction() != null -> CobolStatementType.PERFORMFUNCTION
            cobolStatement.stopoperation() != null -> CobolStatementType.STOPOPERATION
            cobolStatement.ifthenelse() != null -> CobolStatementType.IFTHENELSE
            else -> throw Exception("Unrecognized COBOL statement type!")
        }
    }

    /**
     * Computes a Java statement for a given COBOL statement.
     */
    private fun cobolStatementToJavaStatement(cobolStatement: cobol_grammarParser.StatementContext,
                                              statementType: CobolStatementType): Statement {
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
                cobolPerformTimesTojavaLoop(performtimes)
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
                FunctionCall(
                        name = "STOP",
                        parameters = listOf(),
                        comment = null
                )
            }

            CobolStatementType.IFTHENELSE -> {
                // "IF..THEN..ELSE":
                TODO("not implemented")
            }
        }
    }

    /**
     * Maps a COBOL "PERFORM ... TIMES" loop statement to a Java (for-)loop.
     */
    private fun cobolPerformTimesTojavaLoop(performtimes: cobol_grammarParser.PerformtimesContext?): Statement {
        val cobolLoopCounter = performtimes?.counter() ?: throw NullPointerException("Got a null value from the AST")

        // "inline" (i.e. with a body) or "outline" (i.e. just a function call) PERFORM...TIMES?
        val body: Array<Statement> = if (performtimes.statements() != null) {
            // inline!
            cobolStatementsToJavaStatements(performtimes.statements())
                    .toTypedArray()

        } else {
            // outline!
            val blockname = performtimes.blockname()
            val functionName = blockname.text
            val parameters = listOf<Expression>()
            val functionCall = FunctionCall(
                    name = functionName,
                    parameters = parameters,
                    comment = null
            )
            arrayOf(functionCall)
        }

        // the rule's index and its parent's index are used to create an unique ID:
        val leftHandSide = generateNewInternalIntegerVariable(
                parentRuleIndex = performtimes.parent.ruleIndex,
                ruleIndex = performtimes.ruleIndex,
                idCounter = internalIdCounter,
                knownIDs = knownIDs
        )

        // the loop variable will always start at one for PERFORM..TIMES loops:
        val rightHandSide = "1" // e.g. "for(int i=1; ...) {...}
        val loopVariable = Assignment(
                lhs = leftHandSide,
                rhs = rightHandSide,
                comment = null
        )

        val loopCondition: String = computeForLoopCondition(loopVariable, cobolLoopCounter)
        val loopIncrement = "${loopVariable.lhs.variableName}++"

        return ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = body,
                comment = null
        )
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
     * Computes a Java for-loop condition.
     */
    private fun computeForLoopCondition(loopVariable: Assignment,
                                        cobolLoopCounter: cobol_grammarParser.CounterContext): String {
        val variableName = loopVariable.lhs.variableName

        val counter = cobolLoopCounter.text
        // TODO: ^ this should rather be the transformed variable name!

        return "$variableName<=$counter"
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
        STOPOPERATION,
        IFTHENELSE
    }
}