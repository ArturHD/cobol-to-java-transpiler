package de.netherspace.apps.actojat.ir.java

/**
 * A sum type for all Java expressions: condition | ... | ... .
 */
sealed class Expression(comment: String?) : Statement(comment) { // TODO: this ain't no Statement - is it??

    /**
     * A simple value (e.g. the LHS in "x = 4;").
     */
    class SimpleValue(
            val value: String,
            comment: String?
    ) : Expression(comment)

    /**
     * An arithmetic expression (e.g. "4 * 55").
     */
    class ArithmeticExpression(
            val lhs: String, // TODO: this could be a nested expression!
            val rhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val arithmeticOperator: ArithmeticOperator,
            comment: String?
    ) : Expression(comment) {
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
    class Condition(
            val lhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val rhs: Expression, // TODO: this shouldn't be an arbitrary expression - but ACTOJAT does not enforce a type system right now...
            val conditionalOperator: ConditionalOperator,
            val negated: Boolean,
            comment: String?
    ) : Expression(comment) {
        enum class ConditionalOperator(val literal: String) {
            GREATER(">"),
            LESSER("<"),
            EQUALS("=="),
            GREATEROREQUALS(">="),
            LESSEROREQUALS("<=")
        }
    }

    class GenericExpression( // TODO: this class acts as a placeholder!
            val parts: Array<String>,
            comment: String?
    ) : Expression(comment)
}
