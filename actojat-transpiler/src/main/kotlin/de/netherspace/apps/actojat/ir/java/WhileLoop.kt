package de.netherspace.apps.actojat.ir.java

class WhileLoop (
        val loopCondition: Expression.Condition,
        val evalConditionAtLoopBottom: Boolean,
        body: Sequence<Statement>,
        comment: String?
) : Loop(body = body, comment = comment)
