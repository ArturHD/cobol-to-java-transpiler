package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Clazz
import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct
import org.antlr.v4.runtime.tree.ParseTree
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory
import java.io.File

/**
 * A facade that bundles the different steps the SourceTranspiler
 * interface provides into a single convenience method.
 */
class TranspilerFacade {

    private val log = LoggerFactory.getLogger(TranspilerFacade::class.java)

    /**
     * Transpiles all given files. Returns a list of parse trees and
     * a list of the generated source files.
     *
     * @param sourceFiles a list of source files
     * @param basePackage The desired base package
     * @param transpiler The transpiler implementation (COBOL, C, ...)
     * @param outputDir The folder where the newly generated Java files should be placed
     * @return parse trees and generated source files
     */
    fun transpileFiles(sourceFiles: List<File>,
                       basePackage: String, transpiler: SourceTranspiler,
                       outputDir: File): Pair<List<ParseTree>, List<File>> {
        val parseTrees: List<ParseTree> = sourceFiles
                .asSequence()
                .map { FileUtils.openInputStream(it) }
                .map { transpiler.parseInputStream(it) }
                .filter { it.isSuccess }
                .map { it.getOrThrow() } // TODO: as flatMap?!
                .toList()

        // generated the Java classes (and their class names):
        val generatedJavaClasses: List<Pair<String, String>> = parseTrees
                .asSequence()
                .map { transpiler.generateIntermediateJavaRepresentation(it) }
                .filter { it.isSuccess }
                .map { it.getOrThrow() } // TODO: as flatMap?!
                .flatMap { it.asSequence() }
                .map { Pair(it, className(it)) }
                .map { Pair(transpiler.generateSourceCode(it.first, it.second, basePackage), it.second) }
                .filter { it.first.isSuccess }
                .map { Pair(it.first.getOrThrow(), it.second) } // TODO: as flatMap?!
                .toList()

        // print the generated classes for debugging purposes:
        generatedJavaClasses
                .forEach { log.debug(it.first) }

        val generatedJavaFiles: List<File> = generatedJavaClasses
                .asSequence()
                .map { Pair(transpiler.enrichSourceCode(it.first), it.second) }
                .map {
                    transpiler.writeSingleSourceToFile(
                            code = it.first,
                            dir = computePackageDir(outputDir, basePackage),
                            filename = computeFileName(it.second)
                    )
                }
                .toList()

        return Pair(parseTrees, generatedJavaFiles)
    }

    private fun className(clazz: JavaLanguageConstruct): String {
        return if (clazz !is Clazz) {
            log.error("The given JavaLanguageConstruct is not of type Clazz!")
            ""
        } else {
            clazz.className!!
        }
    }

    private fun computeFileName(source: String): String {
        return "$source.java"
    }

    private fun computePackageDir(outputDir: File, basePackage: String): String {
        val packagePart = basePackage.replace(
                oldChar = '.',
                newChar = '/',
                ignoreCase = true
        )
        // TODO: create a wrapper class around the generated source code that also holds the package!
        // TODO: create the directories under "outputDir" according to the package information!
        return "$outputDir/$packagePart"
    }
}
