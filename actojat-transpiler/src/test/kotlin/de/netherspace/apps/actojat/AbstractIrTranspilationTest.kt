package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.BasicConstruct
import de.netherspace.apps.actojat.ir.java.JavaConstructType
import de.netherspace.apps.actojat.ir.java.Clazz
import org.hamcrest.MatcherAssert.assertThat
import org.slf4j.LoggerFactory
import org.hamcrest.Matchers.`is` as Is

/**
 * This is an abstract test class that provides reusable functions like taking
 * an IR, transpiling it, and checking the generated Java code against a provided
 * expectation.
 *
 * All IR tests can be found in corresponding test classes:
 * - Variables                              -> VariablesIrTranspilationTest
 * - Operators and Assignments              -> OperatorsAndAssignmentsIrTranspilationTest
 * - Expressions, Statements, and Blocks    -> TODO!
 * - Control Flow Statements                -> ControlFlowStatementsIrTranspilationTest
 * - Methods/Functions                      -> ClassesAndMethodsIrTranspilationTest
 */
abstract class AbstractIrTranspilationTest {

    private val log = LoggerFactory.getLogger(AbstractIrTranspilationTest::class.java)
    private val testBasePackage = "actojat.ir.test.pckg"

    /**
     * Some (arbitrary) "system functions".
     *
     * @return a map containing the functions and their IR enum value.
     */
    protected fun systemFunctions(): Map<String, Pair<BasicConstruct, JavaConstructType>> {
        return mapOf(
                "Print" to Pair(BasicConstruct.PRINT, JavaConstructType.FUNCTION),
                "Return" to Pair(BasicConstruct.RETURN, JavaConstructType.KEYWORD)
        )
    }

    /**
     * Performs the actual transpilation test.
     *
     * @param clazz      The intermediate representation
     * @param expectedCode The expected source code after transpilation
     * @throws SourceGenerationException If a source code generation exception occurs
     */
    protected fun doTranspilationTest(clazz: Clazz, expectedCode: String) {
        val sysFunctions = systemFunctions()
        val codeResult = generateCode(clazz, clazz.className!!, testBasePackage, sysFunctions)
        assertThat(codeResult.isSuccess, Is(true))
        val code = codeResult.getOrThrow()
        log.debug(code)
        assertThat(code, Is(expectedCode))
    }

    /**
     * Creates actual source code from an intermediate representation (and adds a package
     *
     * @param clazz         The intermediate representation
     * @param clazzName       The program's name
     * @param basePackage     The desired Java base package
     * @param systemFunctions A map of system functions to canonical Java functions (e.g. "printf")
     * @return A single piece of source code
     * @throws SourceGenerationException If a source code generation exception occurs
     */
    protected fun generateCode(clazz: Clazz, clazzName: String, basePackage: String,
                               systemFunctions: Map<String, Pair<BasicConstruct, JavaConstructType>>): Result<String> {
        val irTranslator = JavaIrToSourceCodeTranslatorImpl(systemFunctions)
        return irTranslator.generateCodeFromIr(clazz, clazzName, basePackage)
    }

}
