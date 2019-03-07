package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.cobol_grammarLexer;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.BasicFunction;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaConstructType;
import de.netherspace.apps.actojat.util.Pair;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;


/**
 * This is the basic parser implementation that parses a single COBOL input source.
 */
@Slf4j
public class CobolSourceTranspilerImpl extends AbstractSourceTranspiler<cobol_grammarLexer,
                                                                cobol_grammarParser,
                                                                cobol_grammarParser.ProgramContext,
                                                                CobolVisitor> {

  private static final Supplier<Map<String, Pair<BasicFunction, JavaConstructType>>>
      systemFunctionsSupplier = () -> {
        HashMap<String, Pair<BasicFunction, JavaConstructType>> map = new HashMap<>();
        map.put("DISPLAY", new Pair<>(BasicFunction.PRINTLN, JavaConstructType.FUNCTION));
        map.put("STOP", new Pair<>(BasicFunction.RETURN, JavaConstructType.KEYWORD));
        map.put("FORLOOP", new Pair<>(BasicFunction.FORLOOP, JavaConstructType.KEYWORD));
        return map;
      };

  /**
   * The default constructor. Supplies all necessary parameters to the super class.
   */
  public CobolSourceTranspilerImpl() {
    super(cobol_grammarLexer::new,
        cobol_grammarParser::new,
        cobol_grammarParser::program,
        CobolVisitor::new,
        systemFunctionsSupplier);

    super.log = log;
  }

}
