package de.netherspace.apps.actojat

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
     * a list of the enerated source code pieces.
     *
     * @param sourceFilesToClassNames source files and their corresponding class names
     * @param basePackage The desired base package
     * @param transpiler The transpiler implementation (COBOL, C, ...)
     * @param outputDir The folder where the newly generated Java files should be placed
     */
    fun transpileFiles(sourceFilesToClassNames: Map<File, String>,
                       basePackage: String, transpiler: SourceTranspiler,
                       outputDir: String): Pair<List<ParseTree>, List<String>> {
        val parseTreesToClassNames = sourceFilesToClassNames
                .asSequence()
                // InputFile x ClassName -> InputStream x ClassName:
                .map { f2cn -> Pair(FileUtils.openInputStream(f2cn.key), f2cn.value) }
                // InputStream x ClassName -> Result<ParseTree> x ClassName:
                .map { is2cn -> Pair(transpiler.parseInputStream(is2cn.first), is2cn.second) }
                .filter { it.first.isSuccess }
                // Result<ParseTree> x ClassName -> ParseTree x ClassName:
                .map { parseTreeResult2cn -> Pair(parseTreeResult2cn.first.getOrThrow(), parseTreeResult2cn.second) } // TODO: as flatMap?!
                .toList()

        val generatedJavaClasses = parseTreesToClassNames
                .asSequence()
                // ParseTree x ClassName -> Result<IR> x ClassName:
                .map { pt2cn -> Pair(transpiler.generateIntermediateJavaRepresentation(pt2cn.first), pt2cn.second) }
                .filter { it.first.isSuccess }
                // Result<IR> x ClassName -> IR x ClassName:
                .map { irr2cn -> Pair(irr2cn.first.getOrThrow(), irr2cn.second) } // TODO: as flatMap?!
                // IR x ClassName -> Result<SourceCode> x ClassName:
                .map { ir2cn -> Pair(transpiler.generateSourceCode(ir2cn.first, ir2cn.second, basePackage), ir2cn.second) }
                .filter { it.first.isSuccess }
                // Result<SourceCode> x ClassName -> SourceCode x ClassName:
                .map { codeResult2cn -> Pair(codeResult2cn.first.getOrThrow(), codeResult2cn.second) } // TODO: as flatMap?!
                .toList()

        // print the generated classes for debugging purposes:
        generatedJavaClasses
                .map { it.first }
                .forEach { sourceCode -> log.debug(sourceCode) }

        val formattedCodeToWrittenFiles = generatedJavaClasses
                .asSequence()
                // SourceCode x ClassName -> EnrichedSourceCode x FileName:
                .map { code2cn -> Pair(transpiler.enrichSourceCode(code2cn.first), computeFileName(code2cn.second)) }
                // EnrichedSourceCode x FileName -> EnrichedSourceCode x File:
                .map { code2fn ->
                    Pair(
                            code2fn.first,
                            transpiler.writeSingleSourceToFile(code2fn.first, computePackageDir(outputDir), code2fn.second)
                    )
                }
                .toList()

        val parseTrees = parseTreesToClassNames
                .map { it.first }
                .toList()

        val formattedJavaClasses = formattedCodeToWrittenFiles
                .map { it.first }
                .toList()

        return Pair(parseTrees, formattedJavaClasses)
        // TODO: the 2 lists inside the Pair to not correlate!
        // TODO: do we really want to return a list of Strings?
    }

    private fun computeFileName(source: String): String {
        return "$source.java"
    }

    private fun computePackageDir(outputDir: String): String {
        // TODO: create a wrapper class around the generated source code that also holds the package!
        // TODO: create the directories under "outputDir" according to the package information!
        TODO("not implemented")
    }

}
