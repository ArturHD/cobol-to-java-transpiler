package de.netherspace.apps.actojat.ir.java

class ConditionalExpr(
        val condition: String, // TODO: should have its own type!
        val thenStatements: List<Statement>, // TODO: should be a (new) type "block"!
        //val elseStatements: List<Statement>, // TODO: should be a (new) type "block"!
        comment: String?
) : Statement(comment)
