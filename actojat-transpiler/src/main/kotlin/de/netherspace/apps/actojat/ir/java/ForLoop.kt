package de.netherspace.apps.actojat.ir.java

class ForLoop(
        val loopVariable: Assignment,
        val loopCondition: Expression.Condition,
        val loopIncrement: Assignment, // TODO: increment and decrement operators should be supported as well!
        body: Sequence<Statement>,
        comment: String?
) : Loop(body = body, comment = comment)
