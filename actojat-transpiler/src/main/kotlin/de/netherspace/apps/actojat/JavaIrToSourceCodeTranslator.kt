package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Program

interface JavaIrToSourceCodeTranslator {

    fun generateCodeFromIr(program: Program, className: String, basePackage: String): String

}
