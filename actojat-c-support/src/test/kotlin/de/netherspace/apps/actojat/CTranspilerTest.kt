package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.c.CSourceTranspilerImpl
import de.netherspace.apps.actojat.util.IntermediateRepresentationException
import de.netherspace.apps.actojat.util.ParserException
import de.netherspace.apps.actojat.util.SourceGenerationException
import org.junit.Ignore
import org.junit.Test
import java.io.IOException
import java.io.InputStream
import java.util.function.Supplier

class CTranspilerTest : AbstractTranspilerTest<CSourceTranspilerImpl>(
        Supplier { CSourceTranspilerImpl() },
        cBasePackage
) {

    companion object {
        private const val cBasePackage = "c.test.pckg"
    }

    /**
     * Tests, whether the transpiler gracefully aborts when a source file is not found.
     *
     * @throws ParserException                     If a parser exception occurs
     * @throws IOException                         If an IO exception occurs
     */
    @Test(expected = IllegalStateException::class)
    fun testCSourceNotFound() {
        val sourceFile = "/c-sources/test-source-thatdoesntexist.c"
        testSourceNotFound(sourceFile)
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
    fun testCHelloWorldTranspilation() {
        val sourceFile = "/c-sources/test-source-helloworld.c"
        val clazzName = "HelloC"
        val expectedCode = "package c.test.pckg;public class HelloC {public void main(){" +
                "System.out.print(\"Hello\");System.out.print(\"World\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("HelloC" to expectedCode)
        )
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
    fun testCSimpleLoopTranspilation() {
        val sourceFile = "/c-sources/test-source-forloop.c"
        val clazzName = "ForLoop"
        val expectedCode = "package c.test.pckg;public class ForLoop {public void main(){for (" +
                "int i=0, i<5, i++) { System.out.print(\"Uh\");System.out.print(\"yeah\"); };return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("ForLoop" to expectedCode)
        )
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
    fun testCSimpleAssignmentTranspilation() {
        val sourceFile = "/c-sources/test-source-simpleassignment.c"
        val clazzName = "SimpleAssignment"
        val expectedCode = "package c.test.pckg;public class SimpleAssignment {" +
                "public void main(){int x=0;x=5;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("SimpleAssignment" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a simple if-then statement.
     *
     * @throws ParserException                     If a parser exception occurs
     * @throws SourceGenerationException           If a source code generation exception occurs
     * @throws IOException                         If an IO exception occurs
     * @throws IntermediateRepresentationException If an IR generation exception occurs
     */
    @Test
    fun testCSimpleIfThenTranspilation() {
        val sourceFile = "/c-sources/test-source-simpleifthen.c"
        val clazzName = "SimpleIfThen"
        val expectedCode = "package c.test.pckg;public class SimpleIfThen {public void main(){" +
                "int x=0;if(x<199){System.out.print(\"Oohhra\");}}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("SimpleIfThen" to expectedCode)
        )
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
    fun testEmptyCFunctionTranspilation() {
        val sourceFile = "/c-sources/test-source-1.c"
        val clazzName = "CTest1"
        val expectedCode = "package c.test.pckg;public class CTest1 {public void main(){}public void bla(){}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("CTest1" to expectedCode)
        )
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
    fun testCImportsTranspilation() {
        val sourceFile = "/c-sources/test-source-3.c"
        val clazzName = "CTest2"
        val expectedCode = "package c.test.pckg;import c.test.pckg.stdio_h;import" +
                " c.test.pckg.test_import1_h;import c.test.pckg.curl_curl_h;import" +
                " c.test.pckg.test_import_with_slash_h;public class CTest2 {public void main(){}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("CTest2" to expectedCode)
        )
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
    fun testSimpleCExpressionsTranspilation() {
        val sourceFile = "/c-sources/test-source-5.c"
        val clazzName = "CTest3"
        val expectedCode = "package c.test.pckg;public class CTest3 {public void main(){bla();}" +
                "public void bla(){a=b+c;System.out.println(\"Hello World\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = clazzName,
                expectations = mapOf("CTest3" to expectedCode)
        )
    }

    /**
     * Loads a source file as input stream.
     *
     * @param sourceFile the source file
     * @return the input stream
     */
    private fun loadSourceFile(sourceFile: String): InputStream {
        val inputStream: InputStream? = CTranspilerTest::class.java.getResourceAsStream(sourceFile)
        return inputStream ?: throw IllegalArgumentException("Source file not found!")
    }
}
