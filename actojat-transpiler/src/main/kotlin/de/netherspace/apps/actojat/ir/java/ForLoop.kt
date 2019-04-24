package de.netherspace.apps.actojat.ir.java

class ForLoop(
        val loopVariable: Assignment,
        val loopCondition: String,
        val loopIncrement: String,
        body: Array<Statement>,
        comment: String?
) : Loop(body = body, comment = comment)
