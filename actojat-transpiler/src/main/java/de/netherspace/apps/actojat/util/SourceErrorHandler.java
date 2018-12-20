package de.netherspace.apps.actojat.util;

import lombok.Getter;
import org.antlr.v4.runtime.ANTLRErrorStrategy;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Token;

/**
 * A custom ErrorHandler that sets a certain error flag whenever an error occurs.
 */
public class SourceErrorHandler extends DefaultErrorStrategy implements ANTLRErrorStrategy {

  @Getter
  private boolean errorFlag;

  /**
   * The default constructor.
   */
  public SourceErrorHandler() {
    super();
    this.errorFlag = false;
  }

  @Override
  public void recover(Parser recognizer,
                      RecognitionException exception) throws RecognitionException {
    this.errorFlag = true;
    super.recover(recognizer, exception);
  }

  @Override
  public void reportError(Parser recognizer, RecognitionException exception) {
    this.errorFlag = true;
    super.reportError(recognizer, exception);
  }

  @Override
  public Token recoverInline(Parser recognizer) throws RecognitionException {
    this.errorFlag = true;
    return super.recoverInline(recognizer);
  }

}
