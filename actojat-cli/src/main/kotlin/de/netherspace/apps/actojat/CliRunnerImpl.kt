package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl
import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl
import org.antlr.v4.gui.TreeViewer
import org.antlr.v4.runtime.tree.ParseTree
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory
import java.awt.Dimension
import java.io.File
import javax.swing.JFrame
import javax.swing.JPanel

class CliRunnerImpl : CliRunner {

    private val log = LoggerFactory.getLogger(CliRunnerImpl::class.java)

    override fun run(sourceFile: String, clazzName: String, basePackage: String, language: App.Language, showGuiTree: Boolean) {
        log.debug("The source file is: $sourceFile")

        val transpiler: SourceTranspiler = when (language) {
            App.Language.COBOL -> CobolSourceTranspilerImpl()
            App.Language.C -> CSourceTranspilerImpl()
        }

        val sourceFiles = listOf(sourceFile) // TODO: change method signature to provide a list!

        val parseTrees = sourceFiles
                .asSequence()
                .map { f -> FileUtils.openInputStream(File(f)) }
                .map { ips -> transpiler.parseInputStream(ips) }
                .filter { parseTreeResult -> parseTreeResult.isSuccess }
                .map { parseTreeResult -> parseTreeResult.getOrThrow() } // TODO: as flatMap?!
                .toList()

        parseTrees
                .asSequence()
                .map { parseTree -> transpiler.generateIntermediateJavaRepresentation(parseTree) }
                .filter { irResult -> irResult.isSuccess }
                .map { irResult -> irResult.getOrThrow() } // TODO: as flatMap?!
                .map { ir -> transpiler.generateSourceCode(ir, clazzName, basePackage) }
                .filter { sourceCodeResult -> sourceCodeResult.isSuccess }
                .map { sourceCodeResult -> sourceCodeResult.getOrThrow() } // TODO: as flatMap?!
                .forEach { sourceCode -> log.debug(sourceCode) }

        if (showGuiTree) {
            val parseTree = parseTrees.first() // TODO: extract to a new/different interface method!
            showSourceCode(transpiler, showGuiTree, parseTree)
        }
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
