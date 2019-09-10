package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.util.SourceGenerationException
import org.slf4j.LoggerFactory

class JavaIrToSourceCodeTranslatorImpl(
        private val systemFunctions: Map<String, Pair<BasicConstruct, JavaConstructType>>

) : JavaIrToSourceCodeTranslator {

    private val log = LoggerFactory.getLogger(JavaIrToSourceCodeTranslatorImpl::class.java)

    private val builder: StringBuilder = StringBuilder()
    private lateinit var sourceMethodNamesToJavaMethods: Map<String, Method>

    override fun generateCodeFromIr(program: Program, className: String, basePackage: String): Result<String> {
        if (className.isEmpty() || basePackage.isEmpty()) {
            val m = "Class name or base package is missing!"
            log.error(m)
            return Result.failure(SourceGenerationException(m))
        }

        append("package $basePackage;")

        // TODO: the IR should rather be organized as a _proper_ tree!
        // TODO: => walk this tree with pattern matching!

        program.imports
                .map { import -> irImportToCode(import, basePackage) }
                .forEach { ic -> append(ic) }

        append("public class $className {")

        program.fields
                .map { field -> irFieldToCode(field) }
                .forEach { ic -> append(ic) }

        sourceMethodNamesToJavaMethods = program.methods // TODO: this looks strange! => refactor?!

        program.methods
                .values
                .map { method -> irMethodToCode(method) }
                .forEach { mc -> append(mc) }

        append("}")
        return Result.success(builder.toString())
    }

    private fun append(s: String) {
        builder.append(s)
    }

    private fun irImportToCode(import: Import, basePackage: String): String {
        return "import $basePackage.${import.name};"
    }

    private fun irFieldToCode(field: Map.Entry<String, Field>): String {
        val fieldModifier = field.value.modifier ?: ""

        return when (val fieldDecl = field.value.declaration) {
            is VariableDeclaration.DeclarationWithoutInit -> {
                val fieldname = fieldDecl.lhs.variableName
                val type: String = computeJavaTypeAnnotationString(fieldDecl.lhs.type
                        ?: throw IllegalArgumentException("The type annotation must not be null!"))
                "$fieldModifier $type $fieldname;"
            }
            is VariableDeclaration.DeclarationWithInit -> {
                val fieldname = fieldDecl.lhs.variableName
                val type: String = computeJavaTypeAnnotationString(fieldDecl.lhs.type
                        ?: throw IllegalArgumentException("The type annotation must not be null!"))
                val initialization = fieldDecl.rhs
                "$fieldModifier $type $fieldname = $initialization;"
            }
        }
    }

    private fun computeJavaTypeAnnotationString(type: Type): String {
        return when (type) {
            is Type.BasicType -> {
                type.primitiveType.typeAnnotation
            }
            is Type.CustomType -> {
                type.typeName ?: throw IllegalArgumentException("The type annotation must not be null!")
            }
        }
    }

    private fun irMethodToCode(method: Method): String {
        val signature = "public void ${method.name}()"
        val body = if (method.statements.isEmpty()) {
            ""
        } else {
            statementsToCode(method.statements)
        }

        return "$signature{$body}"
    }

    private fun statementsToCode(statements: List<Statement>): String {
        return statements
                .map { statement -> statementToCode(statement) }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })
    }

    private fun statementToCode(statement: Statement): String? {
        log.trace("statementToCode() ...")
        log.trace("Statement = $statement")

        return when (statement) {
            is Assignment -> {
                val jassignment = assignmentToCode(statement)
                "$jassignment;"
            }

            is ForLoop -> forLoopToCode(statement)
            is WhileLoop -> whileLoopToCode(statement)
            is FunctionCall -> functionCallToCode(statement)
            is IfThenElse -> ifThenElseToCode(statement)

            else -> {
                log.error("Couldn't match statement!")
                null // TODO: return a Result.failure() instead!
            }
        }
    }

    /**
     * Generates the code for a (single) assignment.
     */
    private fun assignmentToCode(assignment: Assignment): String {
        val rhsCode = expressionsToCode(listOf(assignment.rhs))
        return if (assignment.lhs.type != null) {
            val typeAnnotation = computeJavaTypeAnnotationString(assignment.lhs.type)

            "$typeAnnotation ${assignment.lhs.variableName}=$rhsCode"
        } else {
            "${assignment.lhs.variableName}=$rhsCode"
        }
    }

    /**
     * Generates the Java code for a For-Loop.
     */
    private fun forLoopToCode(forLoop: ForLoop): String {
        val loopVariable = assignmentToCode(forLoop.loopVariable)
        val loopHeader = "for ($loopVariable; ${forLoop.loopCondition}; ${forLoop.loopIncrement})"
        val loopBody = forLoop
                .body
                .map { statement -> statementToCode(statement) }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })

        return "$loopHeader { $loopBody }"
    }

    /**
     * Generates the Java code for a While-Loop.
     */
    private fun whileLoopToCode(whileLoop: WhileLoop): String {
        val condition = expressionsToCode(listOf(whileLoop.loopCondition))
        val loopBody = whileLoop
                .body
                .map { statement -> statementToCode(statement) }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })

        return if (whileLoop.evalConditionAtLoopBottom) {
            "do { $loopBody } while ($condition);"
        } else {
            "while ($condition) { $loopBody }"
        }
    }

    /**
     * Generates the Java code for a function call.
     */
    private fun functionCallToCode(functionCall: FunctionCall): String {
        log.trace("There are ${functionCall.parameters.size} parameters...")

        val parameters = expressionsToCode(functionCall.parameters)
        log.trace("The function's parameters are: '$parameters'")

        // check, whether this source statement maps canonically to a
        // pre-defined Java method (e.g. "DISPLAY" -> "System.out.println"):
        val functionName: String = if (!systemFunctions.containsKey(functionCall.name)) {
            // no, therefore we'll take its original name:
            log.trace("The function's name is: '${functionCall.name}'")
            val m = sourceMethodNamesToJavaMethods[functionCall.name]
                    ?: throw Exception("Couldn't find the source method's name (for key '${functionCall.name}') -> this should not be possible!")
            m.name

        } else {
            // yes, there is a corresponding Java method!
            val systemFunction = systemFunctions[functionCall.name]
                    ?: throw Exception("Couldn't find a system function (for key '${functionCall.name}') -> this should not be possible!")
            val f: BasicConstruct = systemFunction.first
            val constructType: JavaConstructType = systemFunction.second

            // check, if its a _method_ or a mere _keyword_ (e.g. "return"):
            when (constructType) {
                JavaConstructType.KEYWORD -> {
                    val fc = "${f.rawName};"
                    log.trace("The transpiled function call is: '$fc'")
                    return fc
                    // TODO: if its a "return" WITH an associated value (e.g. "return 0" in C), then
                    // TODO: we have to map to "System.exit(value)" iff it's a main method!
                }
                JavaConstructType.FUNCTION -> f.rawName
            }
        }

        val fc = "$functionName($parameters);"
        log.trace("The transpiled function call is: 'fc'")
        return fc
    }

    private fun expressionsToCode(expressions: List<Expression>): String {
        return expressions
                .map { exprToCode(it) }
                .flatMap { it.toList() }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })
    }

    private fun ifThenElseToCode(conditionalExpr: IfThenElse): String {
        val condition = expressionsToCode(listOf(conditionalExpr.condition))
        val thenBody = statementsToCode(conditionalExpr.thenStatements)
        // TODO: else branch!
        return "if($condition){$thenBody}"
    }

    private fun exprToCode(expr: Expression): List<String> {
        return when (expr) {
            is Expression.SimpleValue -> {
                listOf(expr.value)
            }

            is Expression.Condition -> {
                val lhs = expressionsToCode(listOf(expr.lhs))
                val rhs = expressionsToCode(listOf(expr.rhs))
                val cop = expr.conditionalOperator.literal

                // TODO: can there be more than just one (simple) conditional expression?
                if (expr.negated) {
                    listOf("!($lhs$cop$rhs)")
                } else {
                    listOf("$lhs$cop$rhs")
                }
            }

            is Expression.GenericExpression -> {
                expr.parts.toList()
            }

            is Expression.ArithmeticExpression -> {
                val lhs = expr.lhs
                val rhs = expressionsToCode(listOf(expr.rhs))
                val op = expr.arithmeticOperator.literal
                listOf("($lhs$op$rhs)")
            }
        }
    }

}
