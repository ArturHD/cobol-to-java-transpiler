package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.junit.Ignore;
import org.junit.Test;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

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
  }


  /**
   * Tests, whether the transpiler gracefully aborts when a source file is not found.
   *
   * @throws ParserException If a parser exception occurs
   * @throws IOException     If an IO exception occurs
   */
  @Ignore
  @Test(expected = FileNotFoundException.class) // TODO: turn into Result.isSucess check once migrated to Kotlin!
  public void testCSourceNotFound() throws ParserException, IOException {
    final String sourceFile = "c-sources/test-source-thatdoesntexist.c";
    testSourceNotFound(sourceFile);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a Hello World program.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  public void testCHelloWorldTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-helloworld.c";
    final String clazzName = "HelloC";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;public"
        + " class HelloC {public void main(){System.out.print(\"Hello\");"
        + "System.out.print(\"World\");return;}}";
    doTranspilationTest(source, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a simple for-loop.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  @Ignore
  public void testCSimpleLoopTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-forloop.c";
    final String clazzName = "ForLoop";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;public class ForLoop {"
        + "public void main(){for (int i=0, i<5, i++) { "
        + "System.out.print(\"Uh\");System.out.print(\"yeah\"); };return;}}";
    doTranspilationTest(source, clazzName, expectedCode);
  }


  /**
   * Tests, whether the transpiler successfully transpiles a simple assignment.
   *
   * @throws ParserException                     If a parser exception occurs
   * @throws SourceGenerationException           If a source code generation exception occurs
   * @throws IOException                         If an IO exception occurs
   * @throws IntermediateRepresentationException If an IR generation exception occurs
   */
  @Test
  public void testCSimpleAssignmentTranspilation() throws ParserException,
      SourceGenerationException, IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-simpleassignment.c";
    final String clazzName = "SimpleAssignment";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;"
        + "public class SimpleAssignment {public void main(){int x=0;x=5;}}";
    doTranspilationTest(source, clazzName, expectedCode);
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
  @Ignore
  public void testEmptyCFunctionTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-1.c";
    final String clazzName = "CTest1";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;public class CTest1"
        + " {public void main(){}public void bla(){}}";
    doTranspilationTest(source, clazzName, expectedCode);
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
  @Ignore
  public void testCImportsTranspilation() throws ParserException, SourceGenerationException,
      IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-3.c";
    final String clazzName = "CTest2";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;import"
        + " c.test.pckg.test_import1_h;import c.test.pckg.curl_curl_h;import"
        + " c.test.pckg.test_import_with_slash_h;public class CTest2 {public void main(){}}";
    doTranspilationTest(source, clazzName, expectedCode);
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
  @Ignore
  public void testSimpleCExpressionsTranspilation() throws ParserException,
      SourceGenerationException, IOException, IntermediateRepresentationException {
    final String sourceFile = "c-sources/test-source-5.c";
    final String clazzName = "CTest3";
    final InputStream source = loadSourceFile(sourceFile);
    final String expectedCode = "package c.test.pckg;public class CTest3 {public void main(){bla();}"
        + "public void bla(){a=b+c;System.out.println(\"Hello World\");}}";
    doTranspilationTest(source, clazzName, expectedCode);
  }


  /**
   * Loads a source file as input stream.
   *
   * @param sourceFile the source file
   * @return the input stream
   */
  private InputStream loadSourceFile(String sourceFile) {
    // val inputStream = AbstractTranspilerTest::class.java.getResourceAsStream(sourceFile)
    return getClass().getClassLoader().getResourceAsStream(sourceFile);
  }

}
