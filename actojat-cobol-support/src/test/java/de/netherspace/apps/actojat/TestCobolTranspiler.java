package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.junit.Ignore;
import org.junit.Test;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * These are tests to ensure the COBOL transpiler's basics are working.
 */
@Slf4j
public class TestCobolTranspiler extends AbstractTranspilerTest<CobolSourceTranspilerImpl> {

  private static final String cobolBasePackage = "cobol.test.pckg";


  /**
   * The default constructor.
   */
  public TestCobolTranspiler() {
    super(CobolSourceTranspilerImpl::new, cobolBasePackage);
    super.log = log;
  }


  /**
   * Tests, whether the transpiler gracefully aborts when a source file is not found.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws IOException                         If an IO exception occurs
   */
  @Test(expected = FileNotFoundException.class)
  public void testCobolSourceNotFound() throws ParserException, IOException {
    final String sourceFile = "cobol-sources/test-source-thatdoesntexist.cob";
    testSourceNotFound(sourceFile);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a simple Hello-World Cobol program.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  @Ignore
  public void testCobolHelloWorldTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "cobol-sources/test-source-helloworld.cob";
    final String clazzName = "HelloCobol";
    final String expectedCode = "package cobol.test.pckg;public class HelloCobol {public"
        + " void paragraph_DisplayHelloWorld(){System.out.println(\"HelloWorld!\");}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a program containing a simple loop.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  @Ignore
  public void testCobolSimpleLoopTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "cobol-sources/test-source-simpleloop.cob";
    final String clazzName = "SimpleLoop";
    final String expectedCode = "package cobol.test.pckg;public class SimpleLoop {public"
        + " void paragraph_DisplayHelloWorld(){System.out.println(\"HelloWorld!\");}}";
    // TODO: add the exp. String!
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles an (empty) Cobol section.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  @Ignore
  public void testEmptyCobolSectionTranspilation() throws ParserException,
      SourceGenerationException, IOException, IntermediateRepresentationException {
    final String sourceFile = "cobol-sources/test-source-2.cob";
    final String clazzName = "CobolTest1";
    final String expectedCode = "package cobol.test.pckg;public class CobolTest1 {public void"
        + " section_0000_MAIN(){}public void section_0040_DB_CONN(){}public"
        + " void section_0100_INIT(){}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a Cobol import section.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  @Ignore
  public void testCobolImportTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    //final String sourceFile = "cobol-sources/test-source-4.cob";
    final String sourceFile = "cobol-sources/test-source-08.cob";
    final String clazzName = "CobolTest2";
    final String expectedCode = "package cobol.test.pckg;import cobol.test.pckg"
        + ".cobol_TEST_IMPORT_cpy;import cobol.test.pckg.cobol_bla_bli_blubb_cpy;import"
        + " cobol.test.pckg.cobol_log_12340_sql_error_cpy;import cobol.test.pckg"
        + ".cobol_SMURF_SECTIONS_cpy;public class CobolTest2 {public void section_0000_MAIN(){}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }

}
