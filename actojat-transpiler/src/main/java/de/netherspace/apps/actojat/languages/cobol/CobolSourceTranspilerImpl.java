package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.cobol_grammarLexer;
import de.netherspace.apps.actojat.cobol_grammarParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * This is the basic parser implementation that parses a single COBOL input source.
 */
public class CobolSourceTranspilerImpl extends AbstractSourceTranspiler<cobol_grammarLexer, cobol_grammarParser, cobol_grammarParser.ProgramContext, CobolVisitor> {
	
	protected Logger logger;
	
	/**
	 * The default constructor. Supplies all necessary parameters to the super class.
	 */
	public CobolSourceTranspilerImpl() {
		super(charStream -> new cobol_grammarLexer(charStream),
			  commonTokenStream -> new cobol_grammarParser(commonTokenStream),
			  cobol_grammarParser::program,
			  CobolVisitor::new);
		
		this.logger = LogManager.getLogger(CobolSourceTranspilerImpl.class);
		super.logger = this.logger;
	}

}
