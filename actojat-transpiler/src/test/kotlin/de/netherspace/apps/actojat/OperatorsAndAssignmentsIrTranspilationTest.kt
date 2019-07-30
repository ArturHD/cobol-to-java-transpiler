package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import org.junit.Test

/**
 * This test class contains all IR tests regarding the transpilation
 * of operators and assignments.
 */
class OperatorsAndAssignmentsIrTranspilationTest : IrTranspilationTest() {

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
                value = "0"
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
                value = "2"
        )
        val arithmExpr = Expression.ArithmeticExpression(
                lhs = "1", // TODO: should not be a String but a proper type!
                rhs = rhs1,
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
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
                value = "5"
        )
        val arithmExpr2 = Expression.ArithmeticExpression(
                lhs = "100", // TODO: should not be a String but a proper type!
                rhs = rhs1,
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.DIVISION
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
                arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
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

    // TODO: test nested conditional expressions!
    // TODO: test assignments like: 'boolean b = (true || (false && true));'!

}
