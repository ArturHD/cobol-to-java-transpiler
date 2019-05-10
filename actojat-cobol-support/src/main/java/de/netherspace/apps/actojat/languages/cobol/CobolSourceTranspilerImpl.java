package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.AbstractSourceTranspiler;
import de.netherspace.apps.actojat.cobol_grammarLexer;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.ir.java.BasicConstruct;
import de.netherspace.apps.actojat.ir.java.JavaConstructType;
import kotlin.Pair;
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

  private static final Supplier<Map<String, Pair<BasicConstruct, JavaConstructType>>>
      systemFunctionsSupplier = () -> {
        HashMap<String, Pair<BasicConstruct, JavaConstructType>> map = new HashMap<>();
        map.put("DISPLAY", new Pair<>(BasicConstruct.PRINTLN, JavaConstructType.FUNCTION));
        map.put("STOP", new Pair<>(BasicConstruct.RETURN, JavaConstructType.KEYWORD));
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
        systemFunctionsSupplier,
        log);
  }

}
