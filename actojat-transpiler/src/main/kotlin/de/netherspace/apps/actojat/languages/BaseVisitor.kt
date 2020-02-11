package de.netherspace.apps.actojat.languages

import de.netherspace.apps.actojat.ir.java.Expression
import de.netherspace.apps.actojat.ir.java.LeftHandSide
import de.netherspace.apps.actojat.ir.java.PrimitiveType
import de.netherspace.apps.actojat.ir.java.Type
import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicInteger

/**
 * A trait for basic parser visitor operations.
 */
interface BaseVisitor {

    /**
     * Generates the LHS of a new (internal) integer variable declaration
     * (e.g. "int _internal202cb96") which is used for Java IR-internal constructs
     * (e.g. a for-loop that represents a "PERFORM ... TIMES" COBOL statement).
     */
    fun generateNewInternalIntegerVariable(parentRuleIndex: Int,
                                           ruleIndex: Int,
                                           idCounter: AtomicInteger,
                                           knownIDs: MutableList<String>): LeftHandSide {
        val internalId = generateInternalId(
                parentRuleIndex = parentRuleIndex,
                ruleIndex = ruleIndex,
                idCounter = idCounter,
                knownIDs = knownIDs
        )

        return LeftHandSide(
                type = Type.BasicType(PrimitiveType.INT),
                variableName = internalId
        )
    }

    /**
     * Generates a unique identifier.
     */
    private fun generateInternalId(parentRuleIndex: Int,
                                   ruleIndex: Int,
                                   idCounter: AtomicInteger,
                                   knownIDs: MutableList<String>): String {
        val i = idCounter.getAndIncrement()
        val input = "$parentRuleIndex###$ruleIndex##$i"

        val hash = MessageDigest
                .getInstance("MD5")
                .digest(input.toByteArray())
                .asSequence()
                .map { "%02x".format(it) }
                .reduce { acc, s -> acc + s }

        val substr = hash
                .substring(0..6)
                .toUpperCase()
        val newInternalId = "_internal$substr"

        return if (knownIDs.contains(newInternalId)) {
            generateInternalId(
                    parentRuleIndex = parentRuleIndex,
                    ruleIndex = ruleIndex,
                    idCounter = idCounter,
                    knownIDs = knownIDs
            )
        } else {
            knownIDs.add(newInternalId)
            newInternalId
        }
    }

    /**
     * Clones a given conditional expression IR in such a way, that it negates the original expression.
     */
    fun computeNegatedCondition(condition: Expression.Condition): Expression.Condition {
        return Expression.Condition(
                lhs = condition.lhs,
                rhs = condition.rhs,
                conditionalOperator = condition.conditionalOperator,
                negated = !condition.negated
        )
    }

}
