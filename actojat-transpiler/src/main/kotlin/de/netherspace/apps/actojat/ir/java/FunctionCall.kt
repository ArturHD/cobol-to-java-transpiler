package de.netherspace.apps.actojat.ir.java

class FunctionCall(
        /**
         * The function's name (or 'identifier'),
         * e.g. 'doSomething' for this example:
         *   bla = doSomething();
         */
        val name: String,

        val parameters: List<Expression>,
        comment: String?
) : Statement(comment)
