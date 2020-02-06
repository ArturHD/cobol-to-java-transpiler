package de.netherspace.apps.actojat.languages

import de.netherspace.apps.actojat.ir.java.LeftHandSide

class JavaIrUtil {

    companion object {
        /**
         * Strips the type annotation off of a given LHS.
         */
        @JvmStatic
        fun lhsWithoutTypeAnnotation(lhs: LeftHandSide): LeftHandSide {
            return lhs.copy(type = null)
        }
    }
}
