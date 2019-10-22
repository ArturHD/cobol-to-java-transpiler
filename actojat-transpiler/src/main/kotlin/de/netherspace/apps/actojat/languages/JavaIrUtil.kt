package de.netherspace.apps.actojat.languages

import de.netherspace.apps.actojat.ir.java.LeftHandSide

class JavaIrUtil {

    companion object {
        /**
         * Strips the type annotation of a given LHS.
         */
        @JvmStatic
        public fun lhsWithoutTypeAnnotation(lhs: LeftHandSide): LeftHandSide {
            return LeftHandSide(
                    type = null,
                    variableName = lhs.variableName
            )
        }
    }
}
