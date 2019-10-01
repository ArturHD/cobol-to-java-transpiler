package de.netherspace.apps.actojat.ir.java

abstract class Loop (
        val body: Sequence<Statement>,
        comment: String?
) : Statement(comment)
