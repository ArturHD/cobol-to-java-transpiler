package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.*
import org.junit.Test

/**
 * This test class contains all IR tests regarding the transpilation
 * of variable declarations.
 */
class VariablesIrTranspilationTest: IrTranspilationTest() {

    /**
     * Tests the transpilation of global variable declarations.
     */
    @Test
    fun testGlobalVariableDeclarationTranspilation() {
        val field1Name1 = "myFirstField"
        val vardecl1 = VariableDeclaration.DeclarationWithoutInit(
                lhs = LeftHandSide(Type.BasicType(PrimitiveType.INT), field1Name1),
                comment = null
        )
        val field1 = Field(
                modifier = "public", // TODO: should be an enum!
                declaration = vardecl1,
                comment = null
        )

        val field1Name2 = "mySecondField"
        val vardecl2 = VariableDeclaration.DeclarationWithInit(
                lhs = LeftHandSide(Type.BasicType(PrimitiveType.LONG), field1Name2),
                rhs = "99",
                comment = null
        )
        val field2 = Field(
                modifier = "private", // TODO: should be an enum!
                declaration = vardecl2,
                comment = null
        )

        val members = mapOf(
                field1Name1 to field1,
                field1Name2 to field2
        )
        val program = Program(
                methods = mapOf(),
                imports = listOf(),
                fields = members,
                comment = null
        )

        val expectedCode = "package actojat.ir.test.pckg;public class MemberDecl {" +
                "public int myFirstField;private long mySecondField = 99;}"
        doTranspilationTest(program, "MemberDecl", expectedCode)
    }

}
