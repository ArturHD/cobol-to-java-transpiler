package de.netherspace.apps.actojat.languages.cobol

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor
import de.netherspace.apps.actojat.cobol_grammarParser
import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.languages.BaseVisitor
import de.netherspace.apps.actojat.languages.JavaIrUtil
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode
import java.util.concurrent.atomic.AtomicInteger

class CobolVisitor : cobol_grammarBaseVisitor<List<JavaLanguageConstruct>>(), BaseVisitor {

    private var className: String? = null
    private val methods = mutableMapOf<String, Method>()
    private val imports = mutableListOf<Import>()
    private val fields = mutableMapOf<String, Field>()
    private val knownIDs = mutableListOf<String>()
    private val internalIdCounter = AtomicInteger(0)


    override fun visit(tree: ParseTree?): List<JavaLanguageConstruct> {
        super.visit(tree)
        return listOf(Clazz(
                className = className,
                methods = methods,
                imports = imports,
                fields = fields,
                comment = null
        ))
    }

    override fun visitProgramidstatement(ctx: cobol_grammarParser.ProgramidstatementContext?): List<JavaLanguageConstruct> {
        className = ctx?.ID()?.text
        return listOf()
    }

    override fun visitParagraph(ctx: cobol_grammarParser.ParagraphContext?): List<JavaLanguageConstruct> {
        val sourceName: String = ctx
                ?.ID()
                ?.text
                ?: throw NullPointerException("Got a null value from the AST")
        val methodName: String = computeMethodName(ctx)

        val statements: List<Statement> = sentencesToJavaStatements(ctx.sentence())
        val arguments: List<ArgumentDeclaration> = listOf()

        val javaMethod = Method(
                name = methodName,
                statements = statements,
                arguments = arguments,
                comment = null
        )
        methods[sourceName] = javaMethod
        return listOf(javaMethod)
    }

    override fun visitProceduredivision(ctx: cobol_grammarParser.ProceduredivisionContext?): List<JavaLanguageConstruct> {
        return super.visitChildren(ctx)
    }

    override fun visitImportcopyfile(ctx: cobol_grammarParser.ImportcopyfileContext?): List<JavaLanguageConstruct> {
        val importName = computeImportName(
                ctx ?: throw NullPointerException("Got a null value from the AST")
        )
        val jimport = Import(
                name = importName,
                comment = null
        )
        imports.add(jimport)
        return listOf(jimport)
    }

    override fun visitDatadeclaration(ctx: cobol_grammarParser.DatadeclarationContext?): List<JavaLanguageConstruct> {
        val fieldName: String = ctx?.ID()?.text
                ?: throw NullPointerException("Got a null value from the AST")
        // TODO: transform name? (It might not have a valid Java name...)

        val type = cobolPicToJavaType(ctx.datatype())
        val vardecl = if (ctx.datatype()?.initialvalue()?.text != null) {
            VariableDeclaration.DeclarationWithInit(
                    lhs = LeftHandSide(type, fieldName),
                    rhs = ctx.datatype().initialvalue().text,
                    comment = null
            )
        } else {
            VariableDeclaration.DeclarationWithoutInit(
                    lhs = LeftHandSide(type, fieldName),
                    comment = null
            )
        }

        val field = Field(
                modifier = "public", // TODO: should be an enum!
                declaration = vardecl,
                comment = null
        )

        fields[fieldName] = field
        return listOf(field)
    }

    /**
     * Maps a COBOL ("PIC") type to a proper Java type annotation.
     */
    private fun cobolPicToJavaType(pic: cobol_grammarParser.DatatypeContext?): Type {
        val pictureType = pic?.picturetype()?.text
                ?: throw NullPointerException("Got a null value from the AST")

        // the type is given either explicitly (e.g. "PIC 999") or implicitly (e.g. "PIC 9(3)"):
        val size: Int = if (!(pic.size()?.text).isNullOrEmpty()) {
            // it was implicitly:
            val l = pic.size().text.length - 1
            pic.size().text.substring(1, l).toInt()
        } else {
            // ...it was explicitly:
            pictureType.length
        }
        return when (pictureType[0]) {
            '9' -> Type.BasicType(cobolNumericPicToJavaType(size))
            // TODO: handle A, N, X, Z, 1, ... as well!
            'X' -> Type.CustomType("String") // TODO: String should be built-in!
            else -> throw Exception("Unrecognized COBOL picture type!")
        }
    }

    private fun cobolNumericPicToJavaType(size: Int): PrimitiveType {
        return when (size) {
            1 -> PrimitiveType.SHORT
            2 -> PrimitiveType.SHORT
            3 -> PrimitiveType.SHORT
            4 -> PrimitiveType.SHORT
            5 -> PrimitiveType.INT
            6 -> PrimitiveType.INT
            7 -> PrimitiveType.INT
            8 -> PrimitiveType.INT
            9 -> PrimitiveType.INT
            10 -> PrimitiveType.BIGINT
            else -> PrimitiveType.BIGINT // TODO: what is the max for primitive JVM types?
        }
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
     * Computes a list of Java statements for a given list of COBOL sentences.
     */
    private fun sentencesToJavaStatements(sentences: List<cobol_grammarParser.SentenceContext>): List<Statement> {
        return sentences
                .map { sentenceToJavaStatements(it) }
                .fold(listOf(), { acc, list -> acc + list })
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
        // TODO: handle (comment, statement) pairs!
        // TODO: what about multi-line comments?
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
            cobolStatement.assignment() != null -> CobolStatementType.ASSIGNMENT
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

            // "PERFORM ... TIMES":
            CobolStatementType.PERFORMTIMES -> cobolPerformTimesToJavaLoop(cobolStatement.performtimes())

            // "PERFORM ... UNTIL":
            CobolStatementType.PERFORMUNTIL -> cobolPerformUntilToJavaLoop(cobolStatement.performuntil())

            // "PERFORM ... VARYING":
            CobolStatementType.PERFORMVARYING -> cobolPerformVaryingToJavaLoop(cobolStatement.performvarying())

            // "PERFORM ...":
            CobolStatementType.PERFORMFUNCTION -> {
                val functionName = cobolStatement.text
                val parameters = listOf<Expression>()
                FunctionCall(
                        name = functionName,
                        parameters = parameters,
                        comment = null
                )
            }

            // "MOVE ... TO ...":
            CobolStatementType.ASSIGNMENT -> cobolAssignmentToJavaAssignment(cobolStatement.assignment())

            // "STOP ...":
            CobolStatementType.STOPOPERATION -> FunctionCall(
                    name = "STOP",
                    parameters = listOf(),
                    comment = null
            )

            // "IF..THEN..ELSE":
            CobolStatementType.IFTHENELSE -> cobolIfthenelseToJavaConditionalExpr(cobolStatement.ifthenelse())
        }
    }


    /**
     * Maps a COBOL "PERFORM ... UNTIL" loop statement to a Java (while-)loop.
     */
    private fun cobolPerformUntilToJavaLoop(performuntil: cobol_grammarParser.PerformuntilContext?): Statement {
        // we at least need a condition:
        if (performuntil?.condition() == null || performuntil.condition().isEmpty) {
            throw NullPointerException("A condition is missing!")
        }

        // blocknames given or an "inline" body?
        val body: Sequence<Statement> = if (performuntil.statementsorsentences() != null) {
            statementsOrSentencesToJavaStatements(
                    performuntil.statementsorsentences()
            ).asSequence()
        } else {
            // is there a "...THRU..." statement in the loop header?
            if (performuntil.throughblockname() == null || performuntil.throughblockname().isEmpty) {
                // no, only a single one:
                sequenceOf(cobolBlocknameToFunctionCall(performuntil.blockname()))
            } else {
                cobolThruStatementToJavaFunctionCalls(performuntil.blockname(), performuntil.throughblockname().blockname())
            }
        }

        // as COBOL continues to loop UNTIL the condition is met and Java loops WHILE the
        // given condition holds, we have to negate the computed condition:
        val condition = computeNegatedCondition(
                computeCondition(performuntil.condition())
        )

        val doWhileLoop = when {
            performuntil.AFTER() != null -> true
            performuntil.BEFORE() != null -> false
            else -> false
        }

        return WhileLoop(
                loopCondition = condition,
                evalConditionAtLoopBottom = doWhileLoop,
                body = body,
                comment = null
        )
    }

    /**
     * Maps a "PERFORM ... VARYING" loop to a Java for-loop.
     */
    private fun cobolPerformVaryingToJavaLoop(performvarying: cobol_grammarParser.PerformvaryingContext?): Statement {
        // we at least need a condition:
        if (performvarying?.condition() == null || performvarying.condition().isEmpty) {
            throw NullPointerException("A condition is missing!")
        }
        // TODO: does "...THRU,,," work with "...VARYING..."?

        // TODO: handle inline blocks, see cobolPerformUntilToJavaLoop() above!
        val body: Sequence<Statement> = sequenceOf(
                cobolBlocknameToFunctionCall(performvarying.blockname())
                // TODO: an "x = x + A" statement for every VARYING var, , where A is the "BY A" value (or 1)
        )

        val loopVarLhs = LeftHandSide(
                type = null, // the (COBOL) variable was already declared in the data division
                variableName = performvarying.counter().ID().text
        )
        val loopVarRhs = Expression.SimpleValue(performvarying.fromx().NUMBER().text)
        val loopVariable = Assignment(
                lhs = loopVarLhs,
                rhs = loopVarRhs,
                comment = null
        )

        val loopConditionRv: String = performvarying.condition().relationcondition().compval(1).text // TODO: use computeCondition(performvarying.condition()) ?
        val loopCondition = Expression.Condition( // TODO: use computeForLoopCondition(loopVariable, cobolLoopCounterVar) instead!
                lhs = Expression.SimpleValue(loopVariable.lhs.variableName),
                rhs = Expression.SimpleValue(loopConditionRv), // TODO: use computeExpr ?? // TODO: there should be a sum type String x Int x ... !
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSEROREQUALS,
                negated = false
        )

        // TODO: is "BY z" optional? stepwidth=1 might be the default....
        val stepwidth = performvarying.byz().NUMBER().text
        val loopIncrement = Assignment(
                lhs = JavaIrUtil.lhsWithoutTypeAnnotation(loopVarLhs),
                rhs = Expression.ArithmeticExpression(
                        lhs = Expression.SimpleValue(loopVarLhs.variableName), // TODO: should not be a String but a proper type!
                        rhs = Expression.SimpleValue(stepwidth),
                        arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
                ),
                comment = null
        )

        // TODO: a COBOL PERFORM...VARYING statement could result in multiple nested loops!
        // TODO: return a sequenceOf(...) ForLoops instead?!
        return ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = body,
                comment = null
        )
    }

    /**
     * Computes a list of paragraphs for a "startBlockname THRU endBlockname" statement.
     */
    private fun cobolThruStatementToJavaFunctionCalls(startBlockname: cobol_grammarParser.BlocknameContext,
                                                      endBlockname: cobol_grammarParser.BlocknameContext): Sequence<Statement> {
        val firstFunctionToCall = cobolBlocknameToFunctionCall(startBlockname)
        val thruBlocknames = sequenceOf(endBlockname)
        // TODO: this implementation is not correct!
        // TODO: the THRU statement should create a LIST of blocknames: it should contain
        // TODO: (a) all paragraphs inside a given section or
        // TODO: (b) all paragraphs that are written between the startBlockname and endBlock in the
        // TODO: original COBOL source file (as strange as this might sound...)
        // TODO: we therefore have to label all paragraphs with a number and pick all of those, that are
        // TODO: between i=number(startBlock) and j=number(endBlock), including endBlock
        val thruFunctionCalls = thruBlocknames
                .map { cobolBlocknameToFunctionCall(it) }
                .asSequence()
        return sequenceOf(firstFunctionToCall) + thruFunctionCalls
    }

    /**
     * Maps a COBOL "PERFORM ... TIMES" loop statement to a Java (for-)loop.
     */
    private fun cobolPerformTimesToJavaLoop(performtimes: cobol_grammarParser.PerformtimesContext?): Statement {
        val cobolLoopCounterVar = performtimes?.counter()
                ?: throw NullPointerException("Got a null value from the AST")

        // "inline" (i.e. with a body) or "outline" (i.e. just a function call) PERFORM...TIMES?
        val body: Sequence<Statement> = if (performtimes.statementsorsentences() != null) {
            // inline!
            statementsOrSentencesToJavaStatements(
                    performtimes.statementsorsentences())
                    .asSequence()
        } else {
            // outline!
            sequenceOf(cobolBlocknameToFunctionCall(performtimes.blockname()))
        }

        // the rule's index and its parent's index are used to create an unique ID:
        val loopVarLhs = generateNewInternalIntegerVariable(
                parentRuleIndex = performtimes.parent.ruleIndex,
                ruleIndex = performtimes.ruleIndex,
                idCounter = internalIdCounter,
                knownIDs = knownIDs
        )

        // the loop variable will always start at one for PERFORM..TIMES loops:
        val loopVarRhs = Expression.SimpleValue(
                value = "1" // e.g. "for(int i=1; ...) {...}
        )
        val loopVariable = Assignment(
                lhs = loopVarLhs,
                rhs = loopVarRhs,
                comment = null
        )

        val loopCondition = computeForLoopCondition(loopVariable, cobolLoopCounterVar)
        val loopIncrement = Assignment(
                lhs = JavaIrUtil.lhsWithoutTypeAnnotation(loopVarLhs),
                rhs = Expression.ArithmeticExpression(
                        lhs = Expression.SimpleValue(loopVarLhs.variableName), // TODO: should not be a String but a proper type!
                        rhs = Expression.SimpleValue("1"),
                        arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
                ),
                comment = null
        )

        return ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = body,
                comment = null
        )
    }

    /**
     * Creates a Java function call IR for a given COBOL blockname context.
     */
    private fun cobolBlocknameToFunctionCall(blockname: cobol_grammarParser.BlocknameContext): Statement {
        val functionName = blockname.text // TODO: shouldn't this be transformed/checked first?
        return FunctionCall(
                name = functionName,
                parameters = listOf<Expression>(),
                comment = null
        )
    }

    /**
     * Maps a STRINGVALUE node to a Java Expression.
     */
    private fun <N : TerminalNode> stringvalueToJavaExpression(stringvalue: N): Expression {
        val parts = arrayOf(stringvalue.text)
        return Expression.GenericExpression(
                parts = parts
        )
    }

    /**
     * Computes a Java for-loop condition.
     */
    private fun computeForLoopCondition(loopVariable: Assignment,
                                        cobolLoopCounter: cobol_grammarParser.CounterContext): Expression.Condition {
        val counter = cobolLoopCounter.text // TODO: this should rather be the transformed variable name!
        return Expression.Condition(
                lhs = Expression.SimpleValue(loopVariable.lhs.variableName),
                rhs = Expression.SimpleValue(counter), // TODO: there should be a sum type String x Int x ... !
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSEROREQUALS,
                negated = false
        )
    }

    /**
     * Maps a COBOL MOVE-TO statement to a Java assignment.
     */
    private fun cobolAssignmentToJavaAssignment(assignment: cobol_grammarParser.AssignmentContext): Statement {
        val sourceItem = assignment.sourceItem() // TODO: this should be an expr! then: use computeExpr()!
        val rhs: Expression.SimpleValue = when {
            sourceItem.ID() != null -> Expression.SimpleValue(sourceItem.ID().text)
            sourceItem.NUMBER() != null -> Expression.SimpleValue(sourceItem.NUMBER().text)
            sourceItem.STRINGVALUE() != null -> computeCobolStringValueMovement(sourceItem, sourceItem.STRINGVALUE())
            else -> throw Exception("Unrecognized COBOL statement type!")
        }

        val lhs = LeftHandSide(
                type = null,
                variableName = assignment.destItem().ID().text
        )

        return Assignment(
                lhs = lhs,
                rhs = rhs,
                comment = null
        )
    }

    private fun computeCobolStringValueMovement(
            sourceItem: cobol_grammarParser.SourceItemContext,
            destItem: TerminalNode): Expression.SimpleValue {
        // TODO: this is not sufficient:
        return Expression.SimpleValue(destItem.text);

        // TODO: COBOL strings must be truncated or filled up with whitespaces according
        // TODO: to the length of the source and destination!
        // TODO: We can implement this move semantic in two different ways:
        // TODO: a) we add a generic TruncateOrFillUp(src, dest) Java method to
        // TODO: all generated COBOL code bases that does the magic _at runtime_, or
        // TODO: b) add meta-information of the COBOL variable size to our AST nodes
        // TODO: and add proper .substr() calls in the very assignment that we generate
    }

    /**
     * Maps a COBOL if-then-else statement to a Java conditional expression.
     */
    private fun cobolIfthenelseToJavaConditionalExpr(ifthenelse: cobol_grammarParser.IfthenelseContext?): Statement {
        val condition = computeCondition(ifthenelse?.condition()
                ?: throw NullPointerException("Got a null value from the AST"))

        val statements = statementsOrSentencesToJavaStatements(
                ifthenelse
                        .thenblock()
                        .statementsorsentences()
        )

        val elseStatements = if (ifthenelse.elseblock() != null) {
            statementsOrSentencesToJavaStatements(
                    ifthenelse
                            .elseblock()
                            .statementsorsentences()
            )
        } else {
            null
        }

        return IfThenElse(
                condition = condition,
                thenStatements = statements,
                elseStatements = elseStatements,
                comment = null
        )
    }

    /**
     * Computes a Java Condition from a COBOL Condition.
     */
    private fun computeCondition(condition: cobol_grammarParser.ConditionContext): Expression.Condition {
        return when {
            condition.relationcondition() != null -> computeRelationCondition(condition.relationcondition())
            else -> throw Exception("Unrecognized condition!")
        }
    }

    private fun computeRelationCondition(condition: cobol_grammarParser.RelationconditionContext): Expression.Condition {
        val lhs = computeExpr(condition.compval(0))
        val rhs = computeExpr(condition.compval(1))
        val copAndNeg = computeConditionalOperator(condition)
        return Expression.Condition(
                lhs = lhs,
                rhs = rhs,
                conditionalOperator = copAndNeg.first,
                negated = copAndNeg.second
        )
    }

    private fun computeExpr(compval: cobol_grammarParser.CompvalContext): Expression {
        return when { // TODO: there should be a sum type String x Int x ... that is used in the SimpleValue constructor!
            compval.ID() != null -> Expression.SimpleValue(
                    value = compval.ID().text // TODO: IDs should not simply be copied 1:1!
            )

            compval.NUMBER() != null -> Expression.SimpleValue(
                    value = compval.NUMBER().text
            )

            compval.arithmeticexpression() != null -> computeArithmeticExpr(compval.arithmeticexpression())
            // TODO: ...
            else -> throw Exception("Unrecognized value type!")
        }
    }

    private fun computeConditionalOperator(condition: cobol_grammarParser.RelationconditionContext): Pair<Expression.Condition.ConditionalOperator, Boolean> {
        return when {
            condition.comparisonoperator().equalto() != null -> Pair(Expression.Condition.ConditionalOperator.EQUALS, false)
            condition.comparisonoperator().EQUALSIGN() != null -> Pair(Expression.Condition.ConditionalOperator.EQUALS, false)
            condition.comparisonoperator().greaterthan() != null -> Pair(Expression.Condition.ConditionalOperator.GREATER, false)
            condition.comparisonoperator().GREATERSIGN() != null -> Pair(Expression.Condition.ConditionalOperator.GREATER, false)
            condition.comparisonoperator().lessthan() != null -> Pair(Expression.Condition.ConditionalOperator.LESSER, false)
            condition.comparisonoperator().LESSERSIGN() != null -> Pair(Expression.Condition.ConditionalOperator.LESSER, false)
            condition.comparisonoperator().GREATEROREQUALSIGN() != null -> Pair(Expression.Condition.ConditionalOperator.GREATEROREQUALS, false)
            condition.comparisonoperator().greaterthanorequalto() != null -> Pair(Expression.Condition.ConditionalOperator.GREATEROREQUALS, false)
            condition.comparisonoperator().LESSEROREQUALSIGN() != null -> Pair(Expression.Condition.ConditionalOperator.LESSEROREQUALS, false)
            condition.comparisonoperator().lessthanorequalto() != null -> Pair(Expression.Condition.ConditionalOperator.LESSEROREQUALS, false)
            condition.comparisonoperator().notgreaterthan() != null -> Pair(Expression.Condition.ConditionalOperator.GREATER, true)
            condition.comparisonoperator().notgreatersign() != null -> Pair(Expression.Condition.ConditionalOperator.GREATER, true)
            condition.comparisonoperator().notlessthan() != null -> Pair(Expression.Condition.ConditionalOperator.LESSER, true)
            condition.comparisonoperator().notlessersign() != null -> Pair(Expression.Condition.ConditionalOperator.LESSER, true)
            condition.comparisonoperator().notequalto() != null -> Pair(Expression.Condition.ConditionalOperator.EQUALS, true)
            condition.comparisonoperator().notequalsign() != null -> Pair(Expression.Condition.ConditionalOperator.EQUALS, true)
            else -> throw Exception("Unrecognized conditional operator!")
        }
    }

    /**
     * Computes a Java expression from a Cobol arithmetic expr. (e.g. "(c / 2)").
     */
    private fun computeArithmeticExpr(expr: cobol_grammarParser.ArithmeticexpressionContext): Expression.ArithmeticExpression {
        val lhs = computeExpr(expr.compval(0))
        val rhs = computeExpr(expr.compval(1))
        val op: Expression.ArithmeticExpression.ArithmeticOperator = computeArithmeticOperator(expr.arithmeticoperator())

        return Expression.ArithmeticExpression(
                lhs = lhs,
                rhs = rhs,
                arithmeticOperator = op
        )
    }

    private fun computeArithmeticOperator(op: cobol_grammarParser.ArithmeticoperatorContext): Expression.ArithmeticExpression.ArithmeticOperator {
        return when {
            op.ADDSYMBOL() != null -> Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
            op.SUBTRACTSYMBOL() != null -> Expression.ArithmeticExpression.ArithmeticOperator.SUBTRACTION
            op.DIVIDESYMBOL() != null -> Expression.ArithmeticExpression.ArithmeticOperator.DIVISION
            op.MULTIPLYSYMBOL() != null -> Expression.ArithmeticExpression.ArithmeticOperator.MULTIPLICATION
            op.POWERSYMBOL() != null -> throw Exception("The power operator is not (yet) supported!") // TODO: Math.pow()!
            else -> throw Exception("Unrecognized arithmetic operator!")
        }
    }

    /**
     * Distinguishes between a block of statements and a block of sentences. Returns the
     * corresponding Java statements.
     */
    private fun statementsOrSentencesToJavaStatements(ctx: cobol_grammarParser.StatementsorsentencesContext): List<Statement> {
        return when {
            (ctx.sentence() != null && ctx.sentence().isNotEmpty()) -> {
                sentencesToJavaStatements(ctx.sentence())
            }
            (ctx.statements() != null
                    && ctx.statements().statement() != null
                    && ctx.statements().statement().isNotEmpty()) -> {
                cobolStatementsToJavaStatements(ctx.statements())
            }

            else -> throw IllegalArgumentException("Couldn't find any sentences or statements!")
        }
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
        ASSIGNMENT,
        STOPOPERATION,
        IFTHENELSE
    }
}
