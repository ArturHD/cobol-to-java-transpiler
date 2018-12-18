package de.netherspace.apps.actojat.languages.c;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.c_grammarLexer;
import de.netherspace.apps.actojat.c_grammarParser;
import lombok.extern.slf4j.Slf4j;


/**
 * This is the basic parser implementation that parses a single C input source.
 */
@Slf4j
public class CSourceTranspilerImpl extends AbstractSourceTranspiler<c_grammarLexer, c_grammarParser, c_grammarParser.ProgramContext, CVisitor> {

    /**
     * The default constructor. Supplies all necessary parameters to the super class.
     */
    public CSourceTranspilerImpl() {
        super(c_grammarLexer::new,
                c_grammarParser::new,
                c_grammarParser::program,
                CVisitor::new);

        super.log = log;
    }

}