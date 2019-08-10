package de.netherspace.apps.actojat.util

import com.google.googlejavaformat.java.Formatter
import org.slf4j.LoggerFactory

/**
 * A code formatter. Utilizes Google's 'google-java-format' lib.
 */
class CodeFormatter {

    private val log = LoggerFactory.getLogger(CodeFormatter::class.java)

    /**
     * Formats a chunk of Java code with Google's default formatter.
     */
    fun formatCode(code: String): String {
        val formattedSource = Formatter().formatSource(code)
        log.trace("Formatted code is:\n$formattedSource")
        return formattedSource
    }

}
