package de.netherspace.apps.actojat;

import java.io.IOException;

import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import org.junit.Test;


/**
 * These are tests to ensure the COBOL transpiler's basics is working.
 */
public class TestCobolTranspiler extends AbstractTranspilerTest<CobolSourceTranspilerImpl> {

	private static final String cobolBasePackage = "cobol.test.pckg";
	
	/**
	 * The default constructor.
	 */
	public TestCobolTranspiler() {
		super(CobolSourceTranspilerImpl::new, cobolBasePackage);
	}


	/**
	 * Tests, whether the transpiler successfully transpiles an (empty) Cobol section.
	 * 
	 * @throws ParserException If a parser exception occurs
	 * @throws SourceGenerationException If a source code generation exception occurs
	 * @throws IOException If an IO exception occurs
	 * @throws IntermediateRepresentationException If an IR generation exception occurs
	 */
	@Test
	public void testEmptyCobolSectionTranspilation() throws ParserException, SourceGenerationException, IOException, IntermediateRepresentationException {
		String sourceFile = "cobol-sources/test-source-2.cob";
		String clazzName = "CobolTest1";
        String expectedCode = "package cobol.test.pckg;public class CobolTest1 {public void section_0000_MAIN(){}public void section_0040_DB_CONN(){}public void section_0100_INIT(){}}";
		doTranspilationTest(sourceFile, clazzName, expectedCode);
	}
	
	/**
	 * Tests, whether the transpiler successfully transpiles a Cobol import section.
	 * 
	 * @throws ParserException If a parser exception occurs
	 * @throws SourceGenerationException If a source code generation exception occurs
	 * @throws IOException If an IO exception occurs
	 * @throws IntermediateRepresentationException If an IR generation exception occurs
	 */
	@Test
	public void testCobolImportTranspilation() throws ParserException, SourceGenerationException, IOException, IntermediateRepresentationException {
		String sourceFile = "cobol-sources/test-source-4.cob";
		String clazzName = "CobolTest2";
        String expectedCode = "package cobol.test.pckg;import cobol.test.pckg.cobol_TEST_IMPORT_cpy;import cobol.test.pckg.cobol_bla_bli_blubb_cpy;import cobol.test.pckg.cobol_log_12340_sql_error_cpy;import cobol.test.pckg.cobol_SMURF_SECTIONS_cpy;public class CobolTest2 {public void section_0000_MAIN(){}}";
		doTranspilationTest(sourceFile, clazzName, expectedCode);
	}

}
