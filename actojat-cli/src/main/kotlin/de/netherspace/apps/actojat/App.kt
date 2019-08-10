package de.netherspace.apps.actojat

import org.slf4j.LoggerFactory
import java.io.File

class App {

    private val log = LoggerFactory.getLogger(App::class.java)

    private val languageStringToLanguage = mapOf(
            "cobol" to Language.COBOL,
            "c" to Language.C
    )

    fun main(args: Array<String>) {
        if (args.size < 6) {
            log.error("Not enough arguments!")
            return
        }

        val sourceFileOrDir = args[0]
        val clazzName = args[1]
        val basePackage = args[2]
        val languageString = args[3]
        val showGuiTree: Boolean = args[4].toBoolean()
        val outputDir = args[5]

        val language = languageStringToLanguage[languageString.toLowerCase()]
                ?: throw Exception("The language $languageString is not supported!")

        val f = File(sourceFileOrDir)
        val cliRunner: CliRunner = CliRunnerImpl()

        if (f.isDirectory) {
            cliRunner.run(
                    dir = f,
                    basePackage = basePackage,
                    language = language,
                    showGuiTree = showGuiTree,
                    outputDir = outputDir
            )
        } else {
            cliRunner.run(
                    sourceFile = f,
                    clazzName = clazzName,
                    basePackage = basePackage,
                    language = language,
                    showGuiTree = showGuiTree,
                    outputDir = outputDir
            )
        }
    }

    enum class Language(val fileExtensions: List<String>) {
        COBOL(listOf("cob")),
        C(listOf("c"))
    }
}
