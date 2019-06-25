package de.netherspace.apps.actojat.ir.java

class IfThenElse(
        val condition: Expression.Condition,
        val thenStatements: List<Statement>, // TODO: should be a (new) type "block"!
        //val elseStatements: List<Statement>, // TODO: should be a (new) type "block"!
        comment: String?
) : Statement(comment)
