package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;

import java.io.IOException;

/**
 * These are tests to ensure the C transpiler's basics is working.
 */
@Slf4j
public class TestCTranspiler extends AbstractTranspilerTest<CSourceTranspilerImpl> {

  private static final String cBasePackage = "c.test.pckg";


  /**
   * The default constructor.
   */
  public TestCTranspiler() {
    super(CSourceTranspilerImpl::new, cBasePackage);
    super.log = log;
  }


  /**
   * Tests, whether the transpiler successfully transpiles an (empty) C function.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  public void testEmptyCFunctionTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    String sourceFile = "c-sources/test-source-1.c";
    String clazzName = "CTest1";
    String expectedCode = "package c.test.pckg;public class CTest1"
        + " {public void main(){}public void bla(){}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles C imports.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  public void testCImportsTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    String sourceFile = "c-sources/test-source-3.c";
    String clazzName = "CTest2";
    String expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;import"
        + " c.test.pckg.test_import1_h;import c.test.pckg.curl_curl_h;import"
        + " c.test.pckg.test_import_with_slash_h;public class CTest2 {public void main(){}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles simple C assignments and simple
   * function calls.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  public void testSimpleCExpressionsTranspilation() throws ParserException,
      SourceGenerationException, IOException, IntermediateRepresentationException {
    String sourceFile = "c-sources/test-source-5.c";
    String clazzName = "CTest3";
    String expectedCode = "package c.test.pckg;public class CTest3 {public void main(){bla();}"
        + "public void bla(){a=b+c;System.out.println(\"Hello World\");}}";
    doTranspilationTest(sourceFile, clazzName, expectedCode);
  }

}
