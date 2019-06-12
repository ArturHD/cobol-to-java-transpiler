package de.netherspace.apps.actojat.ir.java

/**
 * A sum type for java type annotations: a primitive type | custom type.
 */
sealed class Type {
    class BasicType(val primitiveType: PrimitiveType) : Type()
    class CustomType(val typeName: String?) : Type()
}
