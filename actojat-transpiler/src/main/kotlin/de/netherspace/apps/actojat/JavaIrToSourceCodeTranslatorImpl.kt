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

    override fun generateCodeFromIr(program: Program, className: String, basePackage: String): String {
        if (className.isEmpty() || basePackage.isEmpty()) {
            throw SourceGenerationException() // TODO: return a Result.failure() instead!
        }

        append("package $basePackage;")

        program.imports
                .map { import -> irImportToCode(import, basePackage) }
                .forEach { ic -> append(ic) }

        append("public class $className {")

        sourceMethodNamesToJavaMethods = program.methods // TODO: this looks strange! => refactor?!

        program.methods
                .values
                .map { method -> irMethodToCode(method) }
                .forEach { mc -> append(mc) }

        append("}")
        return builder.toString()
    }

    private fun append(s: String) {
        builder.append(s)
    }

    private fun irImportToCode(import: Import, basePackage: String): String {
        return "import $basePackage.${import.name};"
    }

    private fun irMethodToCode(method: Method): String {
        val signature = "public void ${method.name}()"
        val body = if (method.statements.isEmpty()) {
            ""
        } else {
            method.statements
                    .map { statement -> statementToCode(statement) }
                    .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })
        }

        return "$signature{$body}"
    }

    private fun statementToCode(statement: Statement): String? {
        log.trace("statementToCode() ...")
        log.trace("Statement = $statement")

        return when (statement) {
            is Assignment -> assignmentToCode(statement)
            is ForLoop -> forLoopToCode(statement)
            is FunctionCall -> functionCallToCode(statement)
            else -> {
                log.error("Couldn't match statement!");
                null // TODO: return a Result.failure() instead!
            }
        }
    }

    private fun assignmentToCode(assignment: Assignment): String {
        // TODO: nested expressions!
        return if (assignment.lhs.type != null) {
            "${assignment.lhs.type} ${assignment.lhs.variableName}=${assignment.rhs};"
        } else {
            "${assignment.lhs.variableName}=${assignment.rhs};"
        }
    }

    private fun forLoopToCode(forLoop: ForLoop): String {
        val loopVariable = assignmentToCode(forLoop.loopVariable)
        val loopHeader = "for ($loopVariable, ${forLoop.loopCondition}, ${forLoop.loopIncrement})"
        val loopBody = forLoop
                .body
                .map { statement -> statementToCode(statement) }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })

        return "$loopHeader { $loopBody };"
    }

    private fun functionCallToCode(functionCall: FunctionCall): String {
        log.trace("There are ${functionCall.parameters.size} parameters...")
        val parameters = functionCall
                .parameters
                .map { param -> param.parts } // TODO: correct mapping...
                .flatMap { it.toList() }
                .fold("", { accumulatedBody, stmnt -> accumulatedBody + stmnt })
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

}
