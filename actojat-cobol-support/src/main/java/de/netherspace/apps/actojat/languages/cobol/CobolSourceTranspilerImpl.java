package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.cobol_grammarLexer;
import de.netherspace.apps.actojat.cobol_grammarParser;
import lombok.extern.slf4j.Slf4j;


/**
 * This is the basic parser implementation that parses a single COBOL input source.
 */
@Slf4j
public class CobolSourceTranspilerImpl extends AbstractSourceTranspiler<cobol_grammarLexer, cobol_grammarParser, cobol_grammarParser.ProgramContext, CobolVisitor> {

    /**
     * The default constructor. Supplies all necessary parameters to the super class.
     */
    public CobolSourceTranspilerImpl() {
        super(cobol_grammarLexer::new,
                cobol_grammarParser::new,
                cobol_grammarParser::program,
                CobolVisitor::new);

        super.log = log;
    }

}
