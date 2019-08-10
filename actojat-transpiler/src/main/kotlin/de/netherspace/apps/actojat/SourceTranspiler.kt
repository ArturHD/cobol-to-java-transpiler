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
     * @throws IOException     If an IO exception occurs
     * @throws ParserException If a parser exception occurs
     */
    fun parseInputStream(inputStream: InputStream): Result<ParseTree>


    /**
     * Walks a given parse tree and from this creates an intermediate representation
     * of the target Java source.
     *
     * @param parseTree The parse tree
     * @return The IR as a root level construct (usually a 'program')
     * @throws IntermediateRepresentationException If an IR generation exception occurs
     */
    fun generateIntermediateJavaRepresentation(parseTree: ParseTree): Result<JavaLanguageConstruct>


    /**
     * Creates actual source code from an intermediate representation (and adds a package
     * declaration and outer class).
     *
     * @param program     The intermediate representation
     * @param name        The original filename or program's name, will be used as Java class name
     * @param basePackage The desired Java base package
     * @return A single piece of source code
     * @throws SourceGenerationException If a source code generation exception occurs
     */
    fun generateSourceCode(program: JavaLanguageConstruct, name: String, basePackage: String): Result<String>


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
     * @param The new file's filename
     */
    fun writeSingleSourceToFile(code: String, dir: String, filename: String): File


    /**
     * Returns the rule names.
     *
     * @return The rule names
     */
    fun getRuleNames(): List<String>

}
