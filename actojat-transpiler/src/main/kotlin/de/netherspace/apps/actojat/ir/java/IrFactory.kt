package de.netherspace.apps.actojat.ir.java

class IrFactory {

    fun createProgram(): Program {
        return Program(methods = mutableMapOf(), imports = mutableListOf(), comment = null)
    }

    fun createFunctionCall(name: String): FunctionCall {
        return FunctionCall(name = name, parameters = mutableListOf(), comment = null)
    }

    fun createMethod(name: String): Method {
        return Method(name = name, statements = mutableListOf(), arguments = mutableListOf(), comment = null)
    }

    fun createAssignment(lhs: LeftHandSide, rhs: String): Assignment {
        return Assignment(lhs = lhs, rhs = rhs, comment = null)
    }

    fun createLeftHandSide(type: String, variableName: String): LeftHandSide {
        return LeftHandSide(type = type, variableName = variableName)
    }

    fun createExpression(parts: Array<String>) : Expression {
        return Expression(parts = parts, comment = null)
    }
}
