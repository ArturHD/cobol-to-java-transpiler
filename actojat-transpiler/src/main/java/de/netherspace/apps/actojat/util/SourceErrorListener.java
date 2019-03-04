package de.netherspace.apps.actojat.util;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;

import java.util.BitSet;

/**
 * A Custom ErrorListener that sets a certain error flag whenever an error occurs.
 */
@Slf4j
public class SourceErrorListener extends BaseErrorListener implements ANTLRErrorListener {

  @Getter
  private boolean errorFlag;

  /**
   * The default constructor.
   */
  public SourceErrorListener() {
    super();
    this.errorFlag = false;
  }

  @Override
  public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                          int line, int charPositionInLine,
                          String msg, RecognitionException exception) {
    this.errorFlag = true;
    log.error("Syntax Error found!" + msg);
    super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, exception);
  }

  @Override
  public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex,
                              int stopIndex, boolean exact,
                              BitSet ambigAlts, ATNConfigSet configs) {
    this.errorFlag = true;
    log.error("Ambiguity found!");
    super.reportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs);
  }

}
