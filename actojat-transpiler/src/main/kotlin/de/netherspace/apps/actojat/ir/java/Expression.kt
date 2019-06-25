package de.netherspace.apps.actojat.ir.java

sealed class Expression(comment: String?) : Statement(comment) {

    class Condition(
            val lhs: String, // TODO: this could be a nested expression!
            val rhs: String, // TODO: this could be a nested expression!
            val conditionalOperator: ConditionalOperator,
            comment: String?
    ) : Expression(comment) {
        enum class ConditionalOperator(val literal: String) {
            GREATER(">"),
            LESSER("<"),
            EQUALS("==")
            // TODO: ...
        }
    }

    class GenericExpression( // TODO: this class acts as a placeholder!
            val parts: Array<String>,
            comment: String?
    ) : Expression(comment)
}
