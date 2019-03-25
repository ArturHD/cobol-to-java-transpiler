package de.netherspace.apps.actojat;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.util.function.Supplier;

/**
 * An abstract test class.
 */
public abstract class AbstractTranspilerTest<T extends SourceTranspiler> {

  private Supplier<SourceTranspiler> constructorExpr;
  private String testBasePackage;
  protected Logger log;


  /**
   * The constructor.
   *
   * @param constructorExpr A lambda expression to create a new subclass instance
   * @param testBasePackage The base package that is used during transpilation
   */
  AbstractTranspilerTest(Supplier<SourceTranspiler> constructorExpr, String testBasePackage) {
    super();
    this.constructorExpr = constructorExpr;
    this.testBasePackage = testBasePackage;
  }


  /**
   * Performs an actual transpilation test.
   *
   * @param sourceFile   The source file which is transpiled
   * @param clazzName    The desired class name
   * @param expectedCode The expected source code after transpilation
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  protected void doTranspilationTest(String sourceFile, String clazzName, String expectedCode)
      throws ParserException, SourceGenerationException, IOException,
      IntermediateRepresentationException {
    final SourceTranspiler transpiler = constructorExpr.get();
    final InputStream inputStream = getClass().getClassLoader().getResourceAsStream(sourceFile);
    final ParseTree parseTree = transpiler.parseInputStream(inputStream);
    assertThat(parseTree, is(not(nullValue())));

    final JavaLanguageConstruct ir = transpiler.generateIntermediateJavaRepresentation(parseTree);
    assertThat(ir, is(not(nullValue())));

    final String code = transpiler.generateSourceCode(ir, clazzName, testBasePackage);
    log.debug(code);
    assertThat(expectedCode, is(code));
  }


  /**
   * Performs a transpilation attempt only. Should be used to test whether the underlying
   * abstract transpiler implementation properly handles missing source files.
   *
   * @param sourceFile The source file which should be transpiled
   * @throws ParserException If a parser exception occurs
   * @throws IOException     If an IO exception occurs
   */
  protected void testSourceNotFound(String sourceFile) throws ParserException, IOException {
    final SourceTranspiler transpiler = constructorExpr.get();
    final InputStream inputStream = getClass().getClassLoader().getResourceAsStream(sourceFile);
    transpiler.parseInputStream(inputStream);
  }

}
