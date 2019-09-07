package de.netherspace.apps.actojat.languages.cobol

import de.netherspace.apps.actojat.AbstractSourceTranspiler
import de.netherspace.apps.actojat.cobol_grammarLexer
import de.netherspace.apps.actojat.cobol_grammarParser
import de.netherspace.apps.actojat.ir.java.BasicConstruct
import de.netherspace.apps.actojat.ir.java.JavaConstructType
import org.slf4j.LoggerFactory

class CobolSourceTranspilerImpl : AbstractSourceTranspiler<cobol_grammarLexer,
        cobol_grammarParser, cobol_grammarParser.ProgramContext, CobolVisitor>(
        lexerFactoryExpr = ::cobol_grammarLexer,
        parserFactoryExpr = ::cobol_grammarParser,
        startsymbolExpr = cobol_grammarParser::program,
        visitorFactoryExpr = ::CobolVisitor,
        systemFunctionsSupplier = { systemFunctions },
        log = LoggerFactory.getLogger(CobolSourceTranspilerImpl::class.java)
) {
    companion object {
        private val systemFunctions = mapOf(
                "DISPLAY" to Pair(BasicConstruct.PRINTLN, JavaConstructType.FUNCTION),
                "STOP" to Pair(BasicConstruct.RETURN, JavaConstructType.KEYWORD)
        )
    }
}
