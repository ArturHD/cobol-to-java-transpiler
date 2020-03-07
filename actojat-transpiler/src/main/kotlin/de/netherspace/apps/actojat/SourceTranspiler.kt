package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct
import org.antlr.v4.runtime.tree.ParseTree
import java.io.File
import java.io.InputStream

/**
 * Encapsulates the behavior of a custom parser and source code generator.
 */
interface SourceTranspiler {

    /**
     * Parses a given input stream. Generates a parse tree on success.
     *
     * @param inputStream The input stream
     * @return The parse tree
     */
    fun parseInputStream(inputStream: InputStream): Result<ParseTree>


    /**
     * Walks a given parse tree and from this creates an intermediate representation
     * of the target Java source(s). The source may define more structures that
     * correspond to Java Classes. The resulting list contains all of them.
     *
     * @param parseTree The parse tree
     * @return The IR as a root level construct (usually a 'program') and 0 ore more additional classes
     */
    fun generateIntermediateJavaRepresentation(parseTree: ParseTree): Result<List<JavaLanguageConstruct>>


    /**
     * Creates actual source code from an intermediate representation (and adds a package
     * declaration and outer class).
     *
     * @param clazz     The intermediate representation
     * @param name        The original filename or program's name, will be used as Java class name
     * @param basePackage The desired Java base package
     * @return A single piece of source code
     */
    fun generateSourceCode(clazz: JavaLanguageConstruct, name: String, basePackage: String): Result<String>


    /**
     * Enriches the given source code (e.g. auto-formats the code).
     *
     * @param code The (usually generated) code to enrich
     * @return The full fledged Java code
     */
    fun enrichSourceCode(code: String): String


    /**
     * Writes a single piece of source code to disc.
     *
     * @param code The code to write
     * @param dir The output folder
     * @param filename The new file's filename
     */
    fun writeSingleSourceToFile(code: String, dir: String, filename: String): File


    /**
     * Returns the rule names.
     *
     * @return The rule names
     */
    fun getRuleNames(): List<String>

}
