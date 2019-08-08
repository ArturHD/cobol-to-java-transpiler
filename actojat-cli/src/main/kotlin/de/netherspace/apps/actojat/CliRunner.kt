package de.netherspace.apps.actojat

import java.io.File

/**
 * Encapsulates the command line interface logic.
 */
interface CliRunner {

    /**
     * Performs the transpilation for a single source code file.
     *
     * @param sourceFile  The source code file
     * @param clazzName   The desired class name
     * @param basePackage The desired base package
     * @param language    The chosen language
     * @param showGuiTree If the parse tree should be displayed graphically
     */
    fun run(sourceFile: File, clazzName: String, basePackage: String, language: App.Language, showGuiTree: Boolean)


    /**
     * Performs the transpilation for all files in the given directory.
     * Class names for generated Java classes are derived from the source files'
     * file names.
     *
     * @param dir The directory to (recursively) search for source files.
     * @param basePackage The desired base package
     * @param language    The chosen language
     * @param showGuiTree If the parse tree should be displayed graphically
     */
    fun run(dir: File, basePackage: String, language: App.Language, showGuiTree: Boolean)

}
