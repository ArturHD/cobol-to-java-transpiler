package de.netherspace.apps.actojat.ir.java

class Assignment(
        val lhs: LeftHandSide,
        val rhs: Expression,
        comment: String?
) : Statement(comment)
