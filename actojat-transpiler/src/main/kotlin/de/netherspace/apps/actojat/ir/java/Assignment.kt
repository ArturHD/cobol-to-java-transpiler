package de.netherspace.apps.actojat.ir.java

class Assignment(
        val lhs: LeftHandSide,
        val rhs: String,
        comment: String?
) : Statement(comment)
