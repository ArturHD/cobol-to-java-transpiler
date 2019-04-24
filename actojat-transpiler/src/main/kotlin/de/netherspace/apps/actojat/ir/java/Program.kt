package de.netherspace.apps.actojat.ir.java

class Program(
        val methods: Map<String, Method>,
        val imports: List<Import>,
        comment: String?
) : JavaLanguageConstruct(comment)
