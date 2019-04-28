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

    fun createLeftHandSide(type: String?, variableName: String): LeftHandSide {
        return LeftHandSide(type = type, variableName = variableName)
    }

    fun createExpression(parts: Array<String>): Expression {
        return Expression(parts = parts, comment = null)
    }

    fun createArgumentDeclaration(type: String, name: String): ArgumentDeclaration {
        return ArgumentDeclaration(type = type, name = name, comment = null)
    }

    fun createForLoop(loopVariable: Assignment, loopCondition: String,
                      loopIncrement: String, body: Array<Statement>): ForLoop {
        return ForLoop(loopVariable = loopVariable, loopCondition = loopCondition,
                loopIncrement = loopIncrement, body = body, comment = null)
    }

    fun createImport(name: String): Import {
        return Import(name = name, comment = null)
    }

}
