package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Ignore
import org.junit.Test
import org.slf4j.LoggerFactory
import org.hamcrest.Matchers.`is` as Is

class IrTranspilationTest {

    private val log = LoggerFactory.getLogger(IrTranspilationTest::class.java)
    private val testBasePackage = "actojat.ir.test.pckg"

    @Test
    fun testSimpleFunctionCallTranspilation() {
        // this statement appears inside the method "myMethod" and calls "doSomething":
        val statement1 = FunctionCall(
                name = "doSomethingElse",
                parameters = listOf(),
                comment = null
        )
        val methodName = "myMethod"
        val method = Method(
                name = methodName,
                statements = listOf(statement1),
                arguments = listOf(),
                comment = null
        )

        // the actual "doSomethingElse" method:
        val methodName2 = "doSomethingElse"
        val method2 = Method(
                name = methodName2,
                statements = listOf(),
                arguments = listOf(),
                comment = null
        )

        // the program:
        val methods = mapOf(
                methodName to method,
                methodName2 to method2
        )
        val program = Program(
                methods = methods,
                imports = listOf(),
                comment = null
        )

        val expectedCode: String = "package actojat.ir.test.pckg;public class TestProgram1 {" +
                "public void myMethod(){doSomethingElse();}public void doSomethingElse(){}}"
        doTranspilationTest(program, "TestProgram1", expectedCode)
    }

    @Test
    fun testSimpleAssignmentTranspilation() {
        val type = "int"
        val variableName = "j"
        val lhs = LeftHandSide(
                type = type,
                variableName = variableName
        )
        val rhs = "0"
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = rhs,
                comment = null
        )

        val methodName = "myTestMethod"
        val testMethod = Method(
                name = methodName,
                statements = listOf(assignment1),
                arguments = listOf(),
                comment = null
        )

        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;" +
                "public class SimpleAssignment1 {public void myTestMethod(){int j=0;}}"
        doTranspilationTest(program, "SimpleAssignment1", expectedCode)
    }

    @Test
    fun testHelloWorldTranspilation() {
        // this statement appears inside of "helloWorld" and prints "HelloWorld":
        val expr1 = Expression(
                parts = arrayOf("\"HelloWorld\""),
                comment = null
        )
        val statement1 = FunctionCall(
                name = "Print",
                parameters = listOf(expr1),
                comment = null
        )

        // the "helloWorld" method itself:
        val methodName = "helloWorld"
        val testMethod = Method(
                name = methodName,
                statements = listOf(statement1),
                arguments = listOf(),
                comment = null
        )

        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class HelloWorld {" +
                "public void helloWorld(){System.out.print(\"HelloWorld\");}}"
        doTranspilationTest(program, "HelloWorld", expectedCode)
    }

    @Test
    @Ignore
    fun testForLoopTranspilation() {
        // TODO: !!!
        val b = true
        assertThat(b, Is(true))
    }

    /**
     * Some (arbitrary) "system functions".
     *
     * @return a map containing the functions and their IR enum value.
     */
    fun systemFunctions(): Map<String, Pair<BasicConstruct, JavaConstructType>> {
        return mapOf(
                "Print" to Pair(BasicConstruct.PRINT, JavaConstructType.FUNCTION),
                "Return" to Pair(BasicConstruct.RETURN, JavaConstructType.KEYWORD)
        )
    }

    /**
     * Performs the actual transpilation test.
     *
     * @param program      The intermediate representation
     * @param clazzName    The program's name
     * @param expectedCode The expected source code after transpilation
     * @throws SourceGenerationException If a source code generation exception occurs
     */
    fun doTranspilationTest(program: Program, clazzName: String, expectedCode: String) {
        val sysFunctions = systemFunctions()
        val codeResult = generateCode(program, clazzName, testBasePackage, sysFunctions)
        assertThat(codeResult.isSuccess, Is(true))
        val code = codeResult.getOrThrow()
        log.debug(code)
        assertThat(code, Is(expectedCode))
    }

    /**
     * Creates actual source code from an intermediate representation (and adds a package
     *
     * @param program         The intermediate representation
     * @param clazzName       The program's name
     * @param basePackage     The desired Java base package
     * @param systemFunctions A map of system functions to canonical Java functions (e.g. "printf")
     * @return A single piece of source code
     * @throws SourceGenerationException If a source code generation exception occurs
     */
    fun generateCode(program: Program, clazzName: String, basePackage: String,
                     systemFunctions: Map<String, Pair<BasicConstruct, JavaConstructType>>): Result<String> {
        val irTranslator = JavaIrToSourceCodeTranslatorImpl(systemFunctions)
        return irTranslator.generateCodeFromIr(program, clazzName, basePackage)
    }

}