package de.netherspace.apps.actojat.util

import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory
import java.util.*

/**
 * A Custom ErrorListener that sets an error flag whenever an error occurs.
 */
class SourceErrorListener : BaseErrorListener(), ANTLRErrorListener {

    private val log = LoggerFactory.getLogger(SourceErrorListener::class.java)

    private var errorFlag = false

    override fun syntaxError(recognizer: Recognizer<*, *>?, offendingSymbol: Any?, line: Int, charPositionInLine: Int, msg: String?, e: RecognitionException?) {
        errorFlag = true
        log.error("Syntax Error found:  $msg")
        super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e)
    }

    override fun reportAmbiguity(recognizer: Parser?, dfa: DFA?, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: BitSet?, configs: ATNConfigSet?) {
        errorFlag = true
        log.error("Ambiguity found! startIndex = $startIndex, stopIndex = $stopIndex")
        super.reportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs)
    }

    fun isErrorFlag(): Boolean {
        return errorFlag
    }

}
