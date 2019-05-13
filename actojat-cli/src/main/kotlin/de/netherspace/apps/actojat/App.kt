package de.netherspace.apps.actojat

import org.slf4j.LoggerFactory

class App {

    private val log = LoggerFactory.getLogger(CliRunnerImpl::class.java)

    /**
     * The JVM's entry point...
     *
     * @param args CLI args
     */
    fun main(args: Array<String>) {
        if (args.size < 5) {
            log.error("Not enough arguments!")
            return
        }

        val sourceFile = args[0]
        val clazzName = args[1]
        val basePackage = args[2]
        val languageString = args[3]
        val showGuiTree: Boolean = args[4].toBoolean()

        val language: Language = when (languageString.toLowerCase()) {
            "cobol" -> Language.COBOL
            "c" -> Language.C
            else -> throw Exception("The language $languageString is not supported!")
        }

        // TODO: take multiple files as input!

        val cliRunner: CliRunner = CliRunnerImpl()
        cliRunner.run(
                sourceFile = sourceFile,
                clazzName = clazzName,
                basePackage = basePackage,
                language = language,
                showGuiTree = showGuiTree
        )
    }

    enum class Language {
        COBOL, C
    }
}
