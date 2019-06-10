package de.netherspace.apps.actojat.ir.java

/**
 * A sum type for variable declarations: with initialization | without initialization.
 * Used for both (local) variables as well as (global) fields.
 */
sealed class VariableDeclaration(
        comment: String?
) : JavaLanguageConstruct(comment) {

    class DeclarationWithInit(
            val lhs: LeftHandSide,
            val rhs: String, // TODO: this should rather be a type that allows nested statements!
            comment: String?
    ): VariableDeclaration(comment)

    class DeclarationWithoutInit(
            val lhs: LeftHandSide,
            comment: String?
    ): VariableDeclaration(comment)
}
