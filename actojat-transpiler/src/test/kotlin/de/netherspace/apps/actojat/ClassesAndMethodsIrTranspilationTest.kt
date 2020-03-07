package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Clazz
import de.netherspace.apps.actojat.ir.java.Expression
import de.netherspace.apps.actojat.ir.java.FunctionCall
import de.netherspace.apps.actojat.ir.java.Method
import org.junit.Test

/**
 * This test class contains all IR tests regarding the transpilation
 * of Java classes and methods.
 */
class ClassesAndMethodsIrTranspilationTest : AbstractIrTranspilationTest() {

    /**
     * Tests the transpilation of a trivial function call.
     */
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

        // the program that glues everything together:
        val methods = mapOf(
                methodName to method,
                methodName2 to method2
        )
        val clazz = Clazz(
                className = "TestProgram1",
                methods = methods,
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode: String = "package actojat.ir.test.pckg;public class TestProgram1 {" +
                "public void myMethod(){doSomethingElse();}public void doSomethingElse(){}}"
        doTranspilationTest(clazz, expectedCode)
    }

    /**
     * Tests the transpilation of a very simple "print(HelloWorld)" IR.
     */
    @Test
    fun testHelloWorldTranspilation() {
        // this statement appears inside of "helloWorld" and prints "HelloWorld":
        val expr1 = Expression.GenericExpression(
                parts = arrayOf("\"HelloWorld\"")
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

        // the clazz that glues everything together:
        val clazz = Clazz(
                className = "HelloWorld",
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class HelloWorld {" +
                "public void helloWorld(){System.out.print(\"HelloWorld\");}}"
        doTranspilationTest(clazz, expectedCode)
    }
}
