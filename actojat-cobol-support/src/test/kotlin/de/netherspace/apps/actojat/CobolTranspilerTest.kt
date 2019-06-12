package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl
import de.netherspace.apps.actojat.util.IntermediateRepresentationException
import de.netherspace.apps.actojat.util.ParserException
import de.netherspace.apps.actojat.util.SourceGenerationException
import org.junit.Ignore
import org.junit.Test
import java.io.IOException
import java.io.InputStream
import java.util.function.Supplier

class CobolTranspilerTest : AbstractTranspilerTest<CobolSourceTranspilerImpl>(
        Supplier { CobolSourceTranspilerImpl() },
        cobolBasePackage
) {

    companion object {
        private const val cobolBasePackage = "cobol.test.pckg"
    }

    /**
     * Tests, whether the transpiler gracefully aborts when a source file is not found.
     *
     * @throws ParserException                     If a parser exception occurs
     * @throws IOException                         If an IO exception occurs
     */
    @Test(expected = IllegalStateException::class) // TODO: turn into Result.isSucess check once migrated to Kotlin!
    fun testCobolSourceNotFound() {
        val sourceFile = "/cobol-sources/test-source-thatdoesntexist.cob"
        testSourceNotFound(sourceFile)
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
    fun testCobolHelloWorldTranspilation() {
        val sourceFile = "/cobol-sources/test-source-helloworld.cob"
        val clazzName = "HelloCobol"
        val expectedCode = "package cobol.test.pckg;public class HelloCobol {public" +
                " void paragraph_DisplayHelloWorld(){System.out.println(\"HelloWorld!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
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
    fun testCobolSimpleLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-simpleloop.cob"
        val clazzName = "SimpleLoop"
        val expectedCode = "package cobol.test.pckg;public class SimpleLoop {public void paragraph_MainProgram(){" +
                "for (int _internalDE7D3EA=1; _internalDE7D3EA<=15; _internalDE7D3EA++) { paragraph_DisplayHelloWorld(); };" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a loop with a (global) variable.
     *
     * @throws ParserException                     If a parser exception occurs
     * @throws SourceGenerationException           If a source code generation exception occurs
     * @throws IOException                         If an IO exception occurs
     * @throws IntermediateRepresentationException If an IR generation exception occurs
     */
    @Test
    fun testCobolLoopWithIdTranspilation() {
        val sourceFile = "/cobol-sources/test-source-loopwithid.cob"
        val clazzName = "LoopWithId"
        val expectedCode = "package cobol.test.pckg;public class LoopWithId {public int n = 5;public void paragraph_MainProgram(){" +
                "for (int _internalDE7D3EA=1; _internalDE7D3EA<=n; _internalDE7D3EA++) { paragraph_DisplayHelloWorld(); };" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}" +
                "public void paragraph_DoSomethingElse(){System.out.println(\"Something\");System.out.println(\"else!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
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
    fun testEmptyCobolSectionTranspilation() {
        val sourceFile = "/cobol-sources/test-source-2.cob"
        val clazzName = "CobolTest1"
        val expectedCode = "package cobol.test.pckg;public class CobolTest1 {public void" +
                " section_0000_MAIN(){}public void section_0040_DB_CONN(){}public" +
                " void section_0100_INIT(){}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
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
    fun testCobolImportTranspilation() {
        val sourceFile = "/cobol-sources/test-source-08.cob" //"cobol-sources/test-source-4.cob"
        val clazzName = "CobolTest2"
        val expectedCode = "package cobol.test.pckg;import cobol.test.pckg" +
                ".cobol_TEST_IMPORT_cpy;import cobol.test.pckg.cobol_bla_bli_blubb_cpy;import" +
                " cobol.test.pckg.cobol_log_12340_sql_error_cpy;import cobol.test.pckg" +
                ".cobol_SMURF_SECTIONS_cpy;public class CobolTest2 {public void section_0000_MAIN(){}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Loads a source file as input stream.
     *
     * @param sourceFile the source file
     * @return the input stream
     */
    private fun loadSourceFile(sourceFile: String): InputStream {
        val inputStream: InputStream? = CobolTranspilerTest::class.java.getResourceAsStream(sourceFile)
        return inputStream ?: throw IllegalArgumentException("Source file not found!")
    }

}
