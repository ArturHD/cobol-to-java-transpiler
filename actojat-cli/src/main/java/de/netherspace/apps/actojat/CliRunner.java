package de.netherspace.apps.actojat;

/**
 * Encapsulates the command line interface logic.
 */
public interface CliRunner {

    /**
     * Performs the transpilation for a given source code file.
     *
     * @param sourceFile  The source code file
     * @param clazzName   The desired class name
     * @param basePackage The desired base package
     * @param language    The chosen language
     * @param showGuiTree If the parse tree should be displayed graphically
     * @return true if everything was fine, false else
     */
    boolean run(String sourceFile, String clazzName, String basePackage, App.Language language, boolean showGuiTree);

}
