package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Clazz

interface JavaIrToSourceCodeTranslator {

    fun generateCodeFromIr(clazz: Clazz, className: String, basePackage: String): Result<String>

}
