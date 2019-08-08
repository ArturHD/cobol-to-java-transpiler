package de.netherspace.apps.actojat

import org.antlr.v4.runtime.tree.ParseTree
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory
import java.io.File

class TranspilerFacade {

    private val log = LoggerFactory.getLogger(TranspilerFacade::class.java)

    fun transpileFiles(
            sourceFilesToClassNames: Map<File, String>,
            basePackage: String,
            transpiler: SourceTranspiler
    ): Pair<List<ParseTree>, List<String>> {
        val parseTreesToClassNames = sourceFilesToClassNames
                .asSequence()
                .map { f2cn -> Pair(FileUtils.openInputStream(f2cn.key), f2cn.value) }
                .map { is2cn -> Pair(transpiler.parseInputStream(is2cn.first), is2cn.second) }
                .filter { it.first.isSuccess }
                .map { parseTreeResult2cn -> Pair(parseTreeResult2cn.first.getOrThrow(), parseTreeResult2cn.second) } // TODO: as flatMap?!
                .toList()

        val generatedJavaClasses = parseTreesToClassNames
                .asSequence()
                .map { pt2cn -> Pair(transpiler.generateIntermediateJavaRepresentation(pt2cn.first), pt2cn.second) }
                .filter { it.first.isSuccess }
                .map { irr2cn -> Pair(irr2cn.first.getOrThrow(), irr2cn.second) } // TODO: as flatMap?!
                .map { ir2cn -> transpiler.generateSourceCode(ir2cn.first, ir2cn.second, basePackage) }
                .filter { it.isSuccess }
                .map { sourceCodeResult -> sourceCodeResult.getOrThrow() } // TODO: as flatMap?!
                .toList()

        // print the generated classes for debugging purposes:
        generatedJavaClasses
                .forEach { sourceCode -> log.debug(sourceCode) }

        generatedJavaClasses
                .map { transpiler.enrichSourceCode(it) }
                // TODO: write them to disc!
                .toList()

        val parseTrees = parseTreesToClassNames
                .map { it.first }

        return Pair(parseTrees, generatedJavaClasses)
        // TODO: the 2 lists inside the Pair to not correlate!
        // TODO: do we really want to return a list of Strings?
    }
}
