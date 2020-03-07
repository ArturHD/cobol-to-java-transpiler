package de.netherspace.apps.actojat.ir.java

class Clazz(
        val className: String?,
        val methods: Map<String, Method>,
        val imports: List<Import>,
        val fields: Map<String, Field>,
        comment: String?
) : JavaLanguageConstruct(comment)
