package de.netherspace.apps.actojat.languages.c

import de.netherspace.apps.actojat.AbstractSourceTranspiler
import de.netherspace.apps.actojat.c_grammarLexer
import de.netherspace.apps.actojat.c_grammarParser
import de.netherspace.apps.actojat.ir.java.BasicConstruct
import de.netherspace.apps.actojat.ir.java.JavaConstructType
import org.slf4j.LoggerFactory

class CSourceTranspilerImpl : AbstractSourceTranspiler<c_grammarLexer,
        c_grammarParser, c_grammarParser.ProgramContext, CVisitor>(
        lexerFactoryExpr = ::c_grammarLexer,
        parserFactoryExpr = ::c_grammarParser,
        startsymbolExpr = c_grammarParser::program,
        visitorFactoryExpr = ::CVisitor,
        systemFunctionsSupplier = { systemFunctions },
        log = LoggerFactory.getLogger(CSourceTranspilerImpl::class.java)
) {
    companion object {
        private val systemFunctions = mapOf(
                "printf" to Pair(BasicConstruct.PRINT, JavaConstructType.FUNCTION),
                "return" to Pair(BasicConstruct.RETURN, JavaConstructType.KEYWORD)
        )
    }
}
