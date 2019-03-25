package de.netherspace.apps.actojat;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import de.netherspace.apps.actojat.intermediaterepresentation.java.BasicFunction;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaConstructType;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.util.Pair;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

@Slf4j
public class IrTranspilationTest {

  private static final String testBasePackage = "actojact.ir.test.pckg";

  @Test
  @Ignore
  public void testSimpleAssignmentTranspilation() throws SourceGenerationException {
    final HashMap<String, Pair<BasicFunction, JavaConstructType>> sysFunctions = systemFunctions();
    final String methodName = "myMethod";
    final Method method = new Method(methodName);

    final FunctionCall statement1 = new FunctionCall("doSomethingElse");
    assertThat(method.getStatements(), is(not(nullValue())));
    method.getStatements().add(statement1);

    final Program program = new Program();
    assertThat(program.getMethods(), is(not(nullValue())));
    program.getMethods().put(methodName, method);
    program.getMethods()
        .entrySet()
        .forEach(entry -> System.out.println(" entry "
            + entry.getKey() + " -> "
            + entry.getValue().getName())); // TODO: make an assertion instead!

    final String code = generateCode(program, "TestProgram",
        testBasePackage, sysFunctions);
    assertThat(code, is(not(nullValue())));
    log.debug(code);

    final boolean b = true;
    assertThat(b, is(true));
  }

  @Test
  @Ignore
  public void testHelloWorldTranspilation() {
    // TODO: ...
    final boolean b = true;
    assertThat(b, is(true));
  }

  @Test
  @Ignore
  public void testForLoopTranspilation() {
    // TODO: !!!
    final boolean b = true;
    assertThat(b, is(true));
  }

  /**
   * Some (arbitrary) "system functions".
   *
   * @return a map containing the functions and their IR enum value.
   */
  private HashMap<String, Pair<BasicFunction, JavaConstructType>> systemFunctions() {
    HashMap<String, Pair<BasicFunction, JavaConstructType>> map = new HashMap<>();
    map.put("printf", new Pair<>(BasicFunction.PRINT, JavaConstructType.FUNCTION));
    map.put("return", new Pair<>(BasicFunction.RETURN, JavaConstructType.KEYWORD));
    return map;
  }

  /**
   * Creates actual source code from an intermediate representation (and adds a package
   *
   * @param program         The intermediate representation
   * @param name            The program's name
   * @param basePackage     The desired Java base package
   * @param systemFunctions A map of system functions to canonical Java functions (e.g. "printf")
   * @return A single piece of source code
   * @throws SourceGenerationException If a source code generation exception occurs
   */
  private String generateCode(Program program, String name, String basePackage,
                              Map<String, Pair<BasicFunction, JavaConstructType>> systemFunctions)
      throws SourceGenerationException {
    final JavaIrToSourceCodeTranslator irTranslator
        = new JavaIrToSourceCodeTranslator(systemFunctions);
    irTranslator.setClassName(name);
    irTranslator.setBasePackage(basePackage);
    return irTranslator.generateCodeFromIr(program);
  }

}
