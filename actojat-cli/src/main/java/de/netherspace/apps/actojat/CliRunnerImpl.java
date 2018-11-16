package de.netherspace.apps.actojat;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.swing.JFrame;
import javax.swing.JPanel;

import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl;
import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Implements the command line logic.
 */
public class CliRunnerImpl implements CliRunner {

    private static final Logger logger = LogManager.getLogger(CliRunnerImpl.class);


    @Override
    public boolean run(String sourceFile, String clazzName, String basePackage, App.Language language, boolean showGuiTree) {
        logger.debug("The source file is: " + sourceFile);

        SourceTranspiler transpiler;
        if (language == App.Language.COBOL) {
            transpiler = new CobolSourceTranspilerImpl();
        } else {
            transpiler = new CSourceTranspilerImpl();
        }

        try {
            InputStream inputStream = FileUtils.openInputStream(new File(sourceFile));
            ParseTree parseTree = transpiler.parseInputStream(inputStream);
            JavaLanguageConstruct ir = transpiler.generateIntermediateJavaRepresentation(parseTree);
            String sourceCode = transpiler.generateSourceCode(ir, clazzName, basePackage);
            logger.debug(sourceCode);

            //display parse tree graphically:
            if (showGuiTree) {
                JFrame frame = new JFrame("Parse Tree");
                JPanel panel = new JPanel();
                panel.add(new TreeViewer(transpiler.getRuleNames(), parseTree));
                frame.add(panel);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.setSize(1500, 750);
                frame.setVisible(true);
            }

        } catch (IOException | ParserException | SourceGenerationException | IntermediateRepresentationException e) {
            logger.error("An error occurred:", e);
            return false;
        }

        return true;
    }

}
