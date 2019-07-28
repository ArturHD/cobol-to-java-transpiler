package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test
import org.slf4j.LoggerFactory
import org.hamcrest.Matchers.`is` as Is

class IrTranspilationTest {

    private val log = LoggerFactory.getLogger(IrTranspilationTest::class.java)
    private val testBasePackage = "actojat.ir.test.pckg"

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
        val program = Program(
                methods = methods,
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode: String = "package actojat.ir.test.pckg;public class TestProgram1 {" +
                "public void myMethod(){doSomethingElse();}public void doSomethingElse(){}}"
        doTranspilationTest(program, "TestProgram1", expectedCode)
    }

    /**
     * Tests the transpilation of a simple assignment (int j=0).
     */
    @Test
    fun testSimpleAssignmentTranspilation() {
        // a simple assignment (int j=0):
        val variableName = "j"
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = variableName
        )
        val rhs = Expression.SimpleValue(
                value = "0",
                comment = null
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = rhs,
                comment = null
        )

        // the assignment happens in this trivial method:
        val methodName = "myTestMethod"
        val testMethod = Method(
                name = methodName,
                statements = listOf(assignment1),
                arguments = listOf(),
                comment = null
        )

        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;" +
                "public class SimpleAssignment1 {public void myTestMethod(){int j=0;}}"
        doTranspilationTest(program, "SimpleAssignment1", expectedCode)
    }

    /**
     * Tests the transpilation of a very simple "print(HelloWorld)" IR.
     */
    @Test
    fun testHelloWorldTranspilation() {
        // this statement appears inside of "helloWorld" and prints "HelloWorld":
        val expr1 = Expression.GenericExpression(
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

        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class HelloWorld {" +
                "public void helloWorld(){System.out.print(\"HelloWorld\");}}"
        doTranspilationTest(program, "HelloWorld", expectedCode)
    }


    /**
     * Tests the transpilation of a simple for-loop IR.
     */
    @Test
    fun testForLoopTranspilation() {
        // this statement appears inside of the loop body:
        val expr1 = Expression.GenericExpression(
                parts = arrayOf("\"ImStillLooping\""),
                comment = null
        )
        val printStatement1: Statement = FunctionCall(
                name = "Print",
                parameters = listOf(expr1),
                comment = null
        )

        // the actual loop construct:
        val variableName = "j"
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = variableName
        )
        val rhs = Expression.SimpleValue(
                value = "0",
                comment = null
        )
        val loopVariable = Assignment(
                lhs = lhs,
                rhs = rhs,
                comment = null
        )
        val loopCondition = "j<10" // TODO: this should not be a simple String but rather a type of its own!
        val loopIncrement = "j++" // TODO: this should not be a simple String but rather a type of its own!
        val body = arrayOf(printStatement1)
        val forLoop1 = ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = body,
                comment = null
        )

        // the method containing a for-loop:
        val methodName = "crazyLooping"
        val testMethod = Method(
                name = methodName,
                statements = listOf(forLoop1),
                arguments = listOf(),
                comment = null
        )
        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class ForLoooop {public void crazyLooping()" +
                "{for (int j=0; j<10; j++) { System.out.print(\"ImStillLooping\"); }}}"
        doTranspilationTest(program, "ForLoooop", expectedCode)
    }

    /**
     * Tests the transpilation of global variable declarations.
     */
    @Test
    fun testGlobalVariableDeclarationTranspilation() {
        val field1Name1 = "myFirstField"
        val vardecl1 = VariableDeclaration.DeclarationWithoutInit(
                lhs = LeftHandSide(Type.BasicType(PrimitiveType.INT), field1Name1),
                comment = null
        )
        val field1 = Field(
                modifier = "public", // TODO: should be an enum!
                declaration = vardecl1,
                comment = null
        )

        val field1Name2 = "mySecondField"
        val vardecl2 = VariableDeclaration.DeclarationWithInit(
                lhs = LeftHandSide(Type.BasicType(PrimitiveType.LONG), field1Name2),
                rhs = "99",
                comment = null
        )
        val field2 = Field(
                modifier = "private", // TODO: should be an enum!
                declaration = vardecl2,
                comment = null
        )

        val members = mapOf(
                field1Name1 to field1,
                field1Name2 to field2
        )
        val program = Program(
                methods = mapOf(),
                imports = listOf(),
                fields = members,
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class MemberDecl {" +
                "public int myFirstField;private long mySecondField = 99;}"
        doTranspilationTest(program, "MemberDecl", expectedCode)
    }

    /**
     * Tests the transpilation of a simple conditional expression.
     */
    @Test
    fun testSimpleConditionalExpressionTranspilation() {
        // this statement appears inside of the "then" branch:
        val expr1 = Expression.GenericExpression(
                parts = arrayOf("\"The condition was true!\""),
                comment = null
        )
        val statement1 = FunctionCall(
                name = "Print",
                parameters = listOf(expr1),
                comment = null
        )

        // the variable declaration:
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = "a"
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = Expression.SimpleValue(
                        value = "1",
                        comment = null
                ),
                comment = null
        )

        // ...and the if-then statement itself:
        val lhsA = Expression.SimpleValue(
                value = "a",
                comment = null
        )
        val rhs6 = Expression.SimpleValue(
                value = "6",
                comment = null
        )
        val condition = Expression.Condition(
                lhs = lhsA,
                rhs = rhs6,
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSER,
                negated = true,
                comment = null
        )
        val if1 = IfThenElse(
                condition = condition,
                thenStatements = listOf(statement1),
                comment = null
        )

        // a test method:
        val methodName = "aTestMethod"
        val body = listOf(assignment1, if1)
        val testMethod = Method(
                name = methodName,
                statements = body,
                arguments = listOf(),
                comment = null
        )

        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class SimpleIfThen {public void aTestMethod(){" +
                "int a=1;if(!(a<6)){System.out.print(\"The condition was true!\");}}}"
        doTranspilationTest(program, "SimpleIfThen", expectedCode)
    }

    // TODO: test nested conditional expressions!

    /**
     * Tests the transpilation of a simple arithmetic expression.
     */
    @Test
    fun testArithmeticExpressionTranspilation() {
        // an Integer variable:
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = "x"
        )

        // an expression "1+2":
        val rhs1 = Expression.SimpleValue(
                value = "2",
                comment = null
        )
        val arithmExpr = Expression.ArithmeticExpression(
                lhs = "1", // TODO: should not be a String but a proper type!
                rhs = rhs1,
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION,
                comment = null
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = arithmExpr,
                comment = null
        )

        // a test method:
        val methodName = "m4th"
        val body = listOf(assignment1)
        val testMethod = Method(
                name = methodName,
                statements = body,
                arguments = listOf(),
                comment = null
        )

        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class Arithmetic3xpression {" +
                "public void m4th(){int x=(1+2);}}"
        doTranspilationTest(program, "Arithmetic3xpression", expectedCode)
    }

    /**
     * Tests the transpilation of a nested arithmetic expression.
     */
    @Test
    fun testNestedArithmeticExpressionTranspilation() {
        // the inner expression:
        val rhs1 = Expression.SimpleValue(
                value = "5",
                comment = null
        )
        val arithmExpr2 = Expression.ArithmeticExpression(
                lhs = "100", // TODO: should not be a String but a proper type!
                rhs = rhs1,
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.DIVISION,
                comment = null
        )

        // an Integer variable:
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = "x"
        )

        // the outer expression:
        val arithmExpr = Expression.ArithmeticExpression(
                lhs = "77",
                rhs = arithmExpr2,
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION,
                comment = null
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = arithmExpr,
                comment = null
        )

        // a test method:
        val methodName = "m4th2"
        val body = listOf(assignment1)
        val testMethod = Method(
                name = methodName,
                statements = body,
                arguments = listOf(),
                comment = null
        )

        // the program that glues everything together:
        val program = Program(
                methods = mapOf(methodName to testMethod),
                imports = listOf(),
                fields = mapOf(),
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class NestedArithmetic3xpression {" +
                "public void m4th2(){int x=(77+(100/5));}}"
        doTranspilationTest(program, "NestedArithmetic3xpression", expectedCode)
    }

    /**
     * Some (arbitrary) "system functions".
     *
     * @return a map containing the functions and their IR enum value.
     */
    private fun systemFunctions(): Map<String, Pair<BasicConstruct, JavaConstructType>> {
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
    private fun doTranspilationTest(program: Program, clazzName: String, expectedCode: String) {
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
    private fun generateCode(program: Program, clazzName: String, basePackage: String,
                             systemFunctions: Map<String, Pair<BasicConstruct, JavaConstructType>>): Result<String> {
        val irTranslator = JavaIrToSourceCodeTranslatorImpl(systemFunctions)
        return irTranslator.generateCodeFromIr(program, clazzName, basePackage)
    }

}
