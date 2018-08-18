package de.netherspace.apps.actojat.languages.c;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.c_grammarLexer;
import de.netherspace.apps.actojat.c_grammarParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * This is the basic parser implementation that parses a single C input source.
 */
public class CSourceTranspilerImpl extends AbstractSourceTranspiler<c_grammarLexer, c_grammarParser, c_grammarParser.ProgramContext, CVisitor> {

	protected Logger logger;
	
	/**
	 * The default constructor. Supplies all necessary parameters to the super class.
	 */
	public CSourceTranspilerImpl() {
		super(charStream -> new c_grammarLexer(charStream),
			  commonTokenStream -> new c_grammarParser(commonTokenStream),
			  c_grammarParser::program,
			  CVisitor::new);
		
		this.logger = LogManager.getLogger(CSourceTranspilerImpl.class);
		super.logger = this.logger;
	}

}
