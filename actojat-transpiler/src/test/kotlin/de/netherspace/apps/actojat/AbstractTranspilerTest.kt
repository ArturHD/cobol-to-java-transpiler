package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Program
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.not
import org.hamcrest.Matchers.nullValue
import org.slf4j.LoggerFactory
import java.io.InputStream
import org.hamcrest.Matchers.`is` as Is

/**
 * An abstract test class.
 */
abstract class AbstractTranspilerTest<T>(
        private val constructorExpr: java.util.function.Supplier<SourceTranspiler>,
        private val testBasePackage: String
) where T : SourceTranspiler {

    private val log = LoggerFactory.getLogger(AbstractTranspilerTest::class.java)

    /**
     * Performs an actual transpilation test.
     *
     * @param source       The source which will be transpiled
     * @param clazzName    The desired class name
     * @param expectedCode The expected source code after transpilation
     */
    fun doTranspilationTest(source: InputStream, clazzName: String, expectedCode: String) {
        val transpiler = constructorExpr.get()
        val parseTreeResult = transpiler.parseInputStream(source)
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

        val enrichedCode = transpiler.enrichSourceCode(code)
        assertThat(enrichedCode, Is(not(nullValue())))
    }

    /**
     * Performs a transpilation attempt only. Should be used to test whether the underlying
     * abstract transpiler implementation properly handles missing source files.
     *
     * @param sourceFile The source file which should be transpiled
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
