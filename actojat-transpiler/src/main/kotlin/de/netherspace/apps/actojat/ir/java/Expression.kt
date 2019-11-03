package de.netherspace.apps.actojat.ir.java

/**
 * A sum type for all Java expressions: condition | ... | ... .
 */
sealed class Expression {

    /**
     * A simple value (e.g. the RHS in "x = 4;").
     */
    class SimpleValue(
            val value: String
    ) : Expression()

    /**
     * An arithmetic expression (e.g. "4 * 55").
     */
    class ArithmeticExpression( // TODO: rename the fields "lhs" and "rhs" to "ls" and "rs" respectively -> "right HAND side" should be reserved for assignments only!
            val lhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val rhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val arithmeticOperator: ArithmeticOperator
    ) : Expression() {
        enum class ArithmeticOperator(val literal: String) {
            ADDITION("+"),
            SUBTRACTION("-"),
            MULTIPLICATION("*"),
            DIVISION("/"),
            MODULO("<=")
        }
    }

    /**
     * A condition (e.g. "b == true").
     */
    class Condition( // TODO: rename the fields "lhs" and "rhs" to "ls" and "rs" respectively -> "right HAND side" should be reserved for assignments only!
            val lhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val rhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val conditionalOperator: ConditionalOperator,
            val negated: Boolean
    ) : Expression() {
        enum class ConditionalOperator(val literal: String) {
            GREATER(">"),
            LESSER("<"),
            EQUALS("=="),
            GREATEROREQUALS(">="),
            LESSEROREQUALS("<=")
        }
    }

    class GenericExpression( // TODO: this class acts as a placeholder!
            val parts: Array<String>
    ) : Expression()

}
