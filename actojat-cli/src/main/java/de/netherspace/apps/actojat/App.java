package de.netherspace.apps.actojat;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class App {

  /**
   * The JVM's entry point...
   *
   * @param args CLI args
   */
  public static void main(String[] args) {
    if (args.length < 5) {
      log.error("Not enough arguments!");
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
      log.error("The language '" + languageString + " is not supported!");
      return;
    }

    //TODO: take multiple files as input

    new CliRunnerImpl().run(sourceFile, clazzName, basePackage, language, showGuiTree);
  }

  public enum Language {
    COBOL, C
  }
}
