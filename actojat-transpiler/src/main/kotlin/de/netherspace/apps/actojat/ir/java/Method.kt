package de.netherspace.apps.actojat.ir.java

class Method(
        val name: String,
        val statements: List<Statement>,
        val arguments: List<ArgumentDeclaration>,
        comment: String?
) : JavaLanguageConstruct(comment)
