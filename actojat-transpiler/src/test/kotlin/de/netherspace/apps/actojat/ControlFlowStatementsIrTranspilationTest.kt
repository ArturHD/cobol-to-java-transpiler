package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import de.netherspace.apps.actojat.languages.JavaIrUtil
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
                value = assignment1.lhs.variableName
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
                elseStatements = null,
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
        val printStatement1 = FunctionCall(
                name = "Print",
                parameters = listOf(Expression.SimpleValue(
                        value = "\"ImStillLooping\""
                )),
                comment = null
        )

        // the actual loop construct:
        val loopVarName = "j"
        val loopVarLhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = loopVarName
        )
        val loopVarRhs = Expression.SimpleValue(
                value = "0"
        )
        val loopVariable = Assignment(
                lhs = loopVarLhs,
                rhs = loopVarRhs,
                comment = null
        )
        val loopCondition = Expression.Condition(
                lhs = Expression.SimpleValue("j"), // TODO: this is wrong! "j" is NOT a VALUE! add a variant "identifier" to Expression!
                rhs = Expression.SimpleValue("10"),
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSER,
                negated = false
        )
        val loopIncrement = Assignment( // "j++"
                lhs = JavaIrUtil.lhsWithoutTypeAnnotation(loopVarLhs),
                rhs = Expression.ArithmeticExpression(
                        lhs = Expression.SimpleValue(loopVarLhs.variableName), // TODO: this is wrong! "j" is NOT a VALUE! add a variant "identifier"!
                        rhs = Expression.SimpleValue("1"),
                        arithmeticOperator = Expression.ArithmeticExpression.ArithmeticOperator.ADDITION
                ),
                comment = null
        )
        val forLoop1 = ForLoop(
                loopVariable = loopVariable,
                loopCondition = loopCondition,
                loopIncrement = loopIncrement,
                body = sequenceOf(printStatement1),
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
                "{for (int j=0; j<10; j=(j+1)) { System.out.print(\"ImStillLooping\"); }}}"
        doTranspilationTest(program, "ForLoooop", expectedCode)
    }

    /**
     * Tests the transpilation of a simple while-loop.
     */
    @Test
    fun testWhileLoopTranspilation() {
        // a variable declaration:
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = "varrr"
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = Expression.SimpleValue(
                        value = "2"
                ),
                comment = null
        )

        // the condition for our while-loop:
        val lhsVarrr = Expression.SimpleValue(
                value = assignment1.lhs.variableName
        )
        val rhs6 = Expression.SimpleValue(
                value = "6"
        )
        val condition = Expression.Condition(
                lhs = lhsVarrr,
                rhs = rhs6,
                conditionalOperator = Expression.Condition.ConditionalOperator.LESSER,
                negated = true
        )

        // this statement appears inside of the loop body:
        val printStatement1 = FunctionCall(
                name = "Print",
                parameters = listOf(Expression.SimpleValue(
                        value = "\"Whiiiiiile\""
                )),
                comment = null
        )

        // the actual while loop:
        val whileLoop = WhileLoop(
                loopCondition = condition,
                evalConditionAtLoopBottom = false,
                body = sequenceOf(printStatement1),
                comment = null
        )

        // a method containing our loop:
        val methodName = "testWhileLoop"
        val testMethod = Method(
                name = methodName,
                statements = listOf(assignment1, whileLoop),
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

        val expectedCode = "package actojat.ir.test.pckg;public class AmazingWhileLoop {public void " +
                "testWhileLoop(){int varrr=2;while (!(varrr<6)) { System.out.print(\"Whiiiiiile\"); }}}"
        doTranspilationTest(program, "AmazingWhileLoop", expectedCode)
    }

    /**
     * Tests the transpilation of a do-while-loop.
     */
    @Test
    fun testDoWhileLoopTranspilation() {
        // a variable declaration:
        val lhs = LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = "var2"
        )
        val assignment1 = Assignment(
                lhs = lhs,
                rhs = Expression.SimpleValue(
                        value = "34"
                ),
                comment = null
        )

        // the condition for our while-loop:
        val lhsVar2 = Expression.SimpleValue(
                value = assignment1.lhs.variableName
        )
        val rhs0 = Expression.SimpleValue(
                value = "0"
        )
        val condition = Expression.Condition(
                lhs = lhsVar2,
                rhs = rhs0,
                conditionalOperator = Expression.Condition.ConditionalOperator.GREATEROREQUALS,
                negated = false
        )

        // this statement appears inside of the loop body:
        val printStatement1 = FunctionCall(
                name = "Print",
                parameters = listOf(Expression.SimpleValue(
                        value = "\"My condition is evaluated at the bottom!\""
                )),
                comment = null
        )

        // the do-while loop:
        val doWhileLoop = WhileLoop(
                loopCondition = condition,
                evalConditionAtLoopBottom = true,
                body = sequenceOf(printStatement1),
                comment = null
        )

        // a method containing our loop:
        val methodName = "testDoWhileLoop"
        val testMethod = Method(
                name = methodName,
                statements = listOf(assignment1, doWhileLoop),
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

        val expectedCode = "package actojat.ir.test.pckg;public class DoWhileLoooop {public void " +
                "testDoWhileLoop(){int var2=34;do { System.out.print(\"My condition is evaluated " +
                "at the bottom!\"); } while (var2>=0);}}"
        doTranspilationTest(program, "DoWhileLoooop", expectedCode)
    }

}
