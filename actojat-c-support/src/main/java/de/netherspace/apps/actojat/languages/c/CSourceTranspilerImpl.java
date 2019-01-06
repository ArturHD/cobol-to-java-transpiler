package de.netherspace.apps.actojat.languages.c;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.c_grammarLexer;
import de.netherspace.apps.actojat.c_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.BasicFunction;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;


/**
 * This is the basic parser implementation that parses a single C input source.
 */
@Slf4j
public class CSourceTranspilerImpl extends AbstractSourceTranspiler<c_grammarLexer,
                                                                    c_grammarParser,
                                                                    c_grammarParser.ProgramContext,
                                                                    CVisitor> {

  private static final Supplier<Map<String, BasicFunction>> systemFunctionsSupplier = () -> {
    HashMap<String, BasicFunction> map = new HashMap<>();
    map.put("printf", BasicFunction.PRINTLN);
    return map;
  };

  /**
   * The default constructor. Supplies all necessary parameters to the super class.
   */
  public CSourceTranspilerImpl() {
    super(c_grammarLexer::new,
        c_grammarParser::new,
        c_grammarParser::program,
        CVisitor::new,
        systemFunctionsSupplier);

    super.log = log;
  }

}
