package de.netherspace.apps.actojat.languages.c

import de.netherspace.apps.actojat.c_grammarBaseVisitor
import de.netherspace.apps.actojat.c_grammarParser
import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.languages.BaseVisitor
import org.antlr.v4.runtime.tree.ParseTree
import org.slf4j.LoggerFactory

class CVisitor : c_grammarBaseVisitor<JavaLanguageConstruct>(), BaseVisitor {

    private val log = LoggerFactory.getLogger(CVisitor::class.java)

    private val methods = mutableMapOf<String, Method>()
    private val imports = mutableListOf<Import>()

    override fun visit(tree: ParseTree?): JavaLanguageConstruct {
        super.visit(tree) // TODO: pattern matching instead!
        return Program(
                methods = methods,
                imports = imports,
                fields = mapOf(),
                comment = null
        )
    }

    override fun visitFunctiondeclr(ctx: c_grammarParser.FunctiondeclrContext?): JavaLanguageConstruct {
        val methodName: String = computeMethodName(
                ctx ?: throw NullPointerException("Got a null value from the AST")
        )
        val sourceName = methodName // TODO: this doesn't look right... Fix it!

        /*List<Argument> jarguments = argumentsToJavaArgs.apply(ctx.argumentlist())
                             .entrySet()
                             .stream()
                             .map(argEntryToJavaArgument)
                             .collect(Collectors.toList());
        */
        val arguments: List<ArgumentDeclaration> = listOf()// TODO: fix the above code!

        val statements: List<Statement> = ctx
                .block()
                .expressionlist()
                .expression()
                .map { expressionToJavaStatement(it) }
                .toList()

        val javaMethod = Method(
                name = methodName,
                statements = statements,
                arguments = arguments,
                comment = null
        )
        methods[sourceName] = javaMethod
        return javaMethod // TODO: return a Pair<SourceName, JavaMethod> instead and collect them in a Stream!
    }

    override fun visitImportheader(ctx: c_grammarParser.ImportheaderContext?): JavaLanguageConstruct {
        val importName: String = computeImportName(
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
     * Maps a C function to a Java method's name.
     */
    private fun computeMethodName(ctx: c_grammarParser.FunctiondeclrContext): String {
        return ctx.ID().text
    }

    /**
     * Maps a C expression to a Java statement.
     */
    private fun expressionToJavaStatement(ex: c_grammarParser.ExpressionContext?): Statement {
        if (ex == null) {
            throw NullPointerException("Got a null value from the AST")
        }

        // TODO: distinguish whether its a function call via the grammar!
        // TODO: use a "when"!

        // is it a function call?
        if (ex.functioncall() != null) {
            // set the function's name (e.g. 'doSomething' for 'bla = doSomething();' ):
            val functionName = ex
                    .functioncall()
                    ?.ID()
                    ?.text
                    ?: throw NullPointerException("Got a null value from the AST")

            val parameters = if (ex.functioncall()?.argumentlist() != null) {
                ex.functioncall()
                        .argumentlist()
                        .argument()
                        .map { parameterToJavaExpression(it) }
                        .toList()
            } else {
                listOf()
            }
            return FunctionCall(
                    name = functionName,
                    parameters = parameters,
                    comment = null
            )
        }

        // it's a mere assignment:
        if (ex.assignment() != null) {
            return assignmentToJavaAssignment(ex.assignment())
        }

        // it's a "return":
        if (ex.returnstatement() != null) {
            val functionName = "return" // TODO: create an enum holding these values!
            return FunctionCall(
                    name = functionName,
                    parameters = listOf(),
                    comment = null
            )
        }

        // it's a for-loop:
        if (ex.forloop() != null) {
            return forLoopToJavaForLoop(ex.forloop())
        }

        // it's an if-then-else conditional expr.:
        if (ex.ifthenelse() != null) {
            return ifthenelseToJavaConditionalExpr(ex.ifthenelse())
        }

        throw Exception("couldn't determine statement type:" + ex.text)
    }

    /**
     * Maps a C parameter (e.g. the "HelloWorld" in
     * printf("HelloWorld")
     * ) to a Java expression.
     */
    private fun parameterToJavaExpression(param: c_grammarParser.ArgumentContext?): Expression {
        val parts = arrayOf(param?.text ?: throw NullPointerException("Got a null value from the AST"))
        // TODO: parameters might have to be transformed!
        return Expression(
                parts = parts,
                comment = null
        )
    }

    /**
     * Maps a C assignment to a Java assignment.
     */
    private fun assignmentToJavaAssignment(ctx: c_grammarParser.AssignmentContext?): Assignment {
        val lhs: Pair<Type?, String> = computeLeftHandSide(ctx?.lhs()
                ?: throw NullPointerException("Got a null value from the AST"))

        val lhsType = lhs.first
        val lhsVariableName = lhs.second

        val jlhs = LeftHandSide(
                type = lhsType,
                variableName = lhsVariableName
        )

        val jrhs: String = computeRightHandSide(ctx.rhs())

        return Assignment(
                lhs = jlhs,
                rhs = jrhs,
                comment = null
        )
    }

    /**
     * Maps a left-hand side identifier to a Java identifier: type * name.
     */
    private fun computeLeftHandSide(ctx: c_grammarParser.LhsContext): Pair<Type?, String> {
        return if (ctx.variabledecl() != null) {
            val typeAnnotation = ctx.variabledecl().primitivetype().text // TODO: compute this properly! (Could be a custom type!)
            val variableName = ctx.variabledecl().ID().text

            val cTypesToJavaMap = mapOf(
                    "int" to PrimitiveType.INT
                    // TODO: ...
            )

            val basicType = cTypesToJavaMap[typeAnnotation]
                    ?: throw NoSuchElementException("Couldn't map C type to a Java type!")
            Pair(Type.BasicType(basicType), variableName)
        } else {
            val variableName = ctx.ID().text
            Pair(null, variableName)
        }
    }

    /**
     * Maps a right-hand side to a Java RHS.
     */
    private fun computeRightHandSide(ctx: c_grammarParser.RhsContext): String {
        return ctx.text // TODO: recursively handle nested expressions!
    }

    /**
     * Maps a C for-loop  to a Java for-loop.
     */
    private fun forLoopToJavaForLoop(ctx: c_grammarParser.ForloopContext?): Statement {
        val loopVariable = assignmentToJavaAssignment(
                ctx?.assignment() ?: throw NullPointerException("Got a null value from the AST")
        )

        val loopCondition: String = ctx.condition().text
        val loopIncrement: String = ctx.incrementstatement().text

        val body: Array<Statement> = ctx
                .block()
                .expressionlist()
                .expression()
                .asSequence()
                .map { expressionToJavaStatement(it) }
                .toList()
                .toTypedArray()

        return ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = body,
                comment = null
        )
    }

    private fun ifthenelseToJavaConditionalExpr(ctx: c_grammarParser.IfthenelseContext?): Statement {
        TODO("not implemented")
    }

    /**
     * Maps a C include to a Java's import file name.
     */
    private fun computeImportName(ctx: c_grammarParser.ImportheaderContext): String {
        // TODO: blacklist standard libs (e.g. "<stdio.h>")!
        return ctx
                .FILEID()
                .text
                .replace(Regex("\\."), "_") // import without separating dot
                .replace("/", "_")
                .replace("-", "_")
    }
}
