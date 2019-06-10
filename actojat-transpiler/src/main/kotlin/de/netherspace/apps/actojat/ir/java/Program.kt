package de.netherspace.apps.actojat.ir.java

class Program(
        // TODO: a program should rather be a tree!
        // TODO: Methods, imports etc. should be trees/tree nodes as well!
        val methods: Map<String, Method>,
        val imports: List<Import>,
        val fields: Map<String, Field>,
        comment: String?
) : JavaLanguageConstruct(comment)
