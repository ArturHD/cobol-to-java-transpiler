package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.Clazz
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
     * @param mainClazzName An optional class name
     * @param expectations The expected piece(s) of source code after transpilation
     * @return all generated classes as ordinary strings
     */
    fun doTranspilationTest(source: InputStream, mainClazzName: String?, expectations: Map<String, String>): List<String> {
        val transpiler = constructorExpr.get()
        val parseTreeResult = transpiler.parseInputStream(source)
        parseTreeResult.fold({ parseTree ->
            println(" The parseTree is: $parseTree")
        }, { e ->
            println("Exception was: $e")
        })
        assertThat(parseTreeResult.isSuccess, Is(true))

        val irResult = transpiler.generateIntermediateJavaRepresentation(parseTreeResult.getOrThrow())
        assertThat(irResult.isSuccess, Is(true))
        irResult.fold({ irs ->
            {
                println(" The IR's are:")
                irs.forEach { println(it) }
            }
        }, { e ->
            println("Exception was: $e")
        })

        val generatedClasses = irResult
                .getOrThrow()
                .asSequence()
                .map {
                    val codeResult = transpiler.generateSourceCode(
                            clazz = it as Clazz,
                            name = it.className ?: mainClazzName!!,
                            basePackage = testBasePackage
                    )
                    assertThat(codeResult.isSuccess, Is(true))
                    val code = codeResult.getOrThrow()
                    log.debug(code)
                    code
                }
                .toList()

        log.debug("Checking generated piece(s) of code against expectations...")
        expectations
                .map { (className, expectedCode) ->
                    val code: String = generatedClasses.first { c -> c.contains("class $className") }
                    assertThat(code, Is(expectedCode))
                    className
                }
                .toList()
                .forEach { log.debug("Class '$it' matches the expectation :)") }

        // test code formatting etc.:
        generatedClasses
                .asSequence()
                .map { transpiler.enrichSourceCode(it) }
                .forEach { assertThat(it, Is(not(nullValue()))) }

        // return the "raw" class strings:
        return generatedClasses
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
