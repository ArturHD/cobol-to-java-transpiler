package de.netherspace.apps.actojat;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {

    private static final Logger logger = LogManager.getLogger(App.class);

    public static void main(String[] args) {
        if (args.length < 5) {
            logger.error("Not enough arguments!");
            return;
        }

        String sourceFile = args[0];
        String clazzName = args[1];
        String basePackage = args[2];
        String languageString = args[3];
        boolean showGuiTree = Boolean.parseBoolean(args[4]);

        Language language;
        if (languageString.equalsIgnoreCase("cobol")) {
            language = Language.COBOL;

        } else if (languageString.equalsIgnoreCase("c")) {
            language = Language.C;

        } else {
            logger.error("The language '" + languageString + " is not supported!");
            return;
        }

        //TODO: take multiple files as input

        new CliRunnerImpl().run(sourceFile, clazzName, basePackage, language, showGuiTree);
    }

    public enum Language {
        COBOL, C
    }
}
