package de.netherspace.apps.actojat.util;

import org.antlr.v4.runtime.ANTLRErrorStrategy;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Token;

/**
 * A custom ErrorHandler that sets a certain error flag whenever an error occurs.
 *
 * @author Patrick.Preuss
 */
public class SourceErrorHandler extends DefaultErrorStrategy implements ANTLRErrorStrategy {

	private boolean errorFlag;

	/**
	 * The default constructor.
	 */
	public SourceErrorHandler() {
		super();
		this.errorFlag = false;
	}

	/* (non-Javadoc)
	 * @see org.antlr.v4.runtime.DefaultErrorStrategy#recover(org.antlr.v4.runtime.Parser, org.antlr.v4.runtime.RecognitionException)
	 */
	@Override
	public void recover(Parser recognizer, RecognitionException e) throws RecognitionException {
		this.errorFlag = true;
		super.recover(recognizer, e);
	}
	
	/* (non-Javadoc)
	 * @see org.antlr.v4.runtime.DefaultErrorStrategy#reportError(org.antlr.v4.runtime.Parser, org.antlr.v4.runtime.RecognitionException)
	 */
	@Override
	public void reportError(Parser recognizer, RecognitionException e) {
		this.errorFlag = true;
		super.reportError(recognizer, e);
	}
	
	/* (non-Javadoc)
	 * @see org.antlr.v4.runtime.DefaultErrorStrategy#recoverInline(org.antlr.v4.runtime.Parser)
	 */
	@Override
	public Token recoverInline(Parser recognizer) throws RecognitionException {
		this.errorFlag = true;
		return super.recoverInline(recognizer);
	}

	/**
	 * Returns the error flag.
	 * @return true if an error occurred, false otherwise
	 */
	public boolean isErrorFlag() {
		return errorFlag;
	}

}
