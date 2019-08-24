package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import org.junit.Test

/**
 * This test class contains all IR tests regarding the transpilation
 * of control flow statements (if-then-else, switch, loops).
 */
class ControlFlowStatementsIrTranspilationTest : AbstractIrTranspilationTest() {

    /**
     * Tests the transpilation of a simple If-Then-Else statement.
     */
    @Test
    fun testSimpleIfThenElseTranspilation() {
        // this statement appears inside of the "then" branch:
        val expr1 = Expression.GenericExpression(
                parts = arrayOf("\"The condition was true!\"")
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
                        value = "1"
                ),
                comment = null
        )

        // ...and the if-then statement itself:
        val lhsA = Expression.SimpleValue(
                value = "a"
        )
        val rhs6 = Expression.SimpleValue(
                value = "6"
        )
        val condition = Expression.Condition(
                lhs = lhsA,
                rhs = rhs6,
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSER,
                negated = true
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

    /**
     * Tests the transpilation of a simple for-loop.
     */
    @Test
    fun testForLoopTranspilation() {
        // this statement appears inside of the loop body:
        val expr1 = Expression.SimpleValue(
                value = "\"ImStillLooping\""
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
                value = "0"
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

}
