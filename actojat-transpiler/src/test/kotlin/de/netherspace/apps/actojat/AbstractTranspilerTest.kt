package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Program
import org.hamcrest.MatcherAssert.assertThat
import org.slf4j.LoggerFactory
import org.hamcrest.Matchers.`is` as Is

/**
 * An abstract test class.
 */
abstract class AbstractTranspilerTest(
        private val constructorExpr: java.util.function.Supplier<SourceTranspiler>,
        private val testBasePackage: String
) {
    private val log = LoggerFactory.getLogger(AbstractTranspilerTest::class.java)

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
    fun doTranspilationTest(sourceFile: String, clazzName: String, expectedCode: String) {
        val transpiler = constructorExpr.get()
        val inputStream = AbstractTranspilerTest::class.java.getResourceAsStream(sourceFile)
        val parseTreeResult = transpiler.parseInputStream(inputStream)
        parseTreeResult.fold({ parseTree ->
            println(" The parseTree is: $parseTree")
        }, { e ->
            println("Exception was: $e")
        })
        assertThat(parseTreeResult.isSuccess, Is(true))

        val irResult = transpiler.generateIntermediateJavaRepresentation(parseTreeResult.getOrThrow())
        irResult.fold({ iR ->
            println(" The IR is: $iR")
        }, { e ->
            println("Exception was: $e")
        })
        assertThat(irResult.isSuccess, Is(true))

        val codeResult = transpiler.generateSourceCode(program = irResult.getOrThrow() as Program,
                name = clazzName,
                basePackage = testBasePackage)
        assertThat(codeResult.isSuccess, Is(true))
        val code = codeResult.getOrThrow()
        log.debug(code)
        assertThat(code, Is(expectedCode))
    }

    /**
     * Performs a transpilation attempt only. Should be used to test whether the underlying
     * abstract transpiler implementation properly handles missing source files.
     *
     * @param sourceFile The source file which should be transpiled
     * @throws ParserException If a parser exception occurs
     * @throws IOException     If an IO exception occurs
     */
    fun testSourceNotFound(sourceFile: String) {
        val transpiler = constructorExpr.get()
        val inputStream = AbstractTranspilerTest::class.java.getResourceAsStream(sourceFile)
        val parseTreeResult = transpiler.parseInputStream(inputStream)
        parseTreeResult.fold({ parseTree ->
            println(" The parseTree is: $parseTree")
        }, { e ->
            println("Exception was: $e")
        })
        assertThat(parseTreeResult.isSuccess, Is(true))
    }

}
