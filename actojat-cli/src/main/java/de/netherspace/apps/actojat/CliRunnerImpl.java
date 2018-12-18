package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl;
import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.io.FileUtils;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

/**
 * Implements the command line logic.
 */
@Slf4j
public class CliRunnerImpl implements CliRunner {

    @Override
    public boolean run(String sourceFile, String clazzName, String basePackage, App.Language language, boolean showGuiTree) {
        log.debug("The source file is: " + sourceFile);

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
            log.debug(sourceCode);

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
            log.error("An error occurred:", e);
            return false;
        }

        return true;
    }

}
