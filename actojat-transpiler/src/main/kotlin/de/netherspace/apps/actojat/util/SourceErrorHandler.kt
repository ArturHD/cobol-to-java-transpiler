package de.netherspace.apps.actojat.util

import org.antlr.v4.runtime.*

/**
 * A custom ErrorHandler that sets an error flag whenever an error occurs.
 */
class SourceErrorHandler : DefaultErrorStrategy(), ANTLRErrorStrategy {

    private var errorFlag = false

    override fun recover(recognizer: Parser?, e: RecognitionException?) {
        errorFlag = true
        super.recover(recognizer, e)
    }

    override fun reportError(recognizer: Parser?, e: RecognitionException?) {
        errorFlag = true
        super.reportError(recognizer, e)
    }

    override fun recoverInline(recognizer: Parser?): Token {
        errorFlag = true
        return super.recoverInline(recognizer)
    }

    fun isErrorFlag(): Boolean {
        return errorFlag
    }

}
