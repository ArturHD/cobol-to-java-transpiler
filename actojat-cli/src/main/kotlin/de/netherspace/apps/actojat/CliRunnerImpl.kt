package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl
import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl
import org.antlr.v4.gui.TreeViewer
import org.antlr.v4.runtime.tree.ParseTree
import org.slf4j.LoggerFactory
import java.awt.Dimension
import java.io.File
import javax.swing.JFrame
import javax.swing.JPanel

/**
 * Default CliRunner implementation: Transpiles a given source file
 * (or all source files within a folder) and allows to display their
 * parse trees in a GUI.
 */
class CliRunnerImpl : CliRunner {

    private val log = LoggerFactory.getLogger(CliRunnerImpl::class.java)

    override fun run(sourceFile: File, clazzName: String, basePackage: String,
                     language: App.Language, showGuiTree: Boolean, outputDir: File): List<File> {
        log.debug("The source file is: ${sourceFile.absolutePath}")
        val sourceFilesToClassNames = mapOf(
                sourceFile to clazzName
        )
        return transpileFilesAndShowParseTree(
                sourceFilesToClassNames = sourceFilesToClassNames,
                basePackage = basePackage,
                language = language,
                showGuiTree = showGuiTree,
                outputDir = outputDir
        )
    }

    override fun run(dir: File, basePackage: String, language: App.Language,
                     showGuiTree: Boolean, outputDir: File): List<File> {
        log.debug("The folder containing the source files is: ${dir.absolutePath}")
        dir.walk()
                // TODO:
//                .filter { d -> d.isFile }
//                .filter { d -> d.extension == language.name }
                .forEach { println("f: $it") }

        return emptyList() // TODO: transpileFilesAndShowParseTree(...)
    }

    private fun transpileFilesAndShowParseTree(sourceFilesToClassNames: Map<File, String>,
                                               basePackage: String, language: App.Language,
                                               showGuiTree: Boolean, outputDir: File): List<File> {
        val transpiler: SourceTranspiler = when (language) {
            App.Language.COBOL -> CobolSourceTranspilerImpl()
            App.Language.C -> CSourceTranspilerImpl()
        }

        val result = TranspilerFacade().transpileFiles(
                sourceFilesToClassNames = sourceFilesToClassNames,
                basePackage = basePackage,
                transpiler = transpiler,
                outputDir = outputDir
        )

        val parseTrees = result.first
        if (showGuiTree) {
            val parseTree = parseTrees.first() // TODO: extract to a new/different interface method!
            showSourceCode(transpiler, showGuiTree, parseTree)
        }

        return result.second
    }

    private fun showSourceCode(transpiler: SourceTranspiler, showGuiTree: Boolean, parseTree: ParseTree) {
        //display parse tree graphically:
        val frame = JFrame("Parse Tree")
        val panel = JPanel()
        panel.add(TreeViewer(transpiler.getRuleNames(), parseTree))
        frame.add(panel)
        frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        frame.size = Dimension(1500, 750)
        frame.isVisible = true
    }

}
