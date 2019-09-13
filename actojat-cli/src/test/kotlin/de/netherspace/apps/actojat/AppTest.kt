package de.netherspace.apps.actojat

import org.hamcrest.MatcherAssert.assertThat
import org.junit.Ignore
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import java.io.File
import org.hamcrest.Matchers.`is` as Is

class AppTest {

    @get:Rule
    val temporaryFolder = TemporaryFolder() // TODO: creates a temp. folder under /tmp/ -> should rather be under /target!

    @Test
    fun testTranspileSingleFile() {
        val sourceFilePath = "/cobol-sources/hellocobol.cob"
        val sourceFileResource: String = AppTest::class.java.getResource(sourceFilePath).file
        val sourceFile = File(sourceFileResource)
        val outputDir = temporaryFolder.newFolder("cobol-outputdir")

        val generatedSourceFiles = CliRunnerImpl().run(
                sourceFile = sourceFile,
                clazzName = "TestClazz56565",
                basePackage = "my.base.pckg",
                language = App.Language.COBOL,
                showGuiTree = false,
                outputDir = outputDir
        )
        assertThat(generatedSourceFiles.isEmpty(), Is(false))
    }

    @Test
    @Ignore
    fun testTranspileAllSourcesInADirectory() {
        val b = true
        assertThat(b, Is(true))
        // TODO: test the transpilation of all sources within a directory!
    }

    @Test
    fun testTranspilePerformTimes() {
        assertThatTranspiledCodeMatches(
                sourceFilePath = "/cobol-sources/performtimes.cob",
                expectedSourceFilePath = "/expected-java-sources/PerformTimes.java",
                clazzname = "PerformTimesTest"
        )
    }

    @Test
    @Ignore
    fun testTranspilePerformUntil() {
        assertThatTranspiledCodeMatches(
                sourceFilePath = "/cobol-sources/abc.cob", // TODO: create proper source code...
                expectedSourceFilePath = "/expected-java-sources/xyz.java", // TODO: create proper source code...
                clazzname = "PerformUntilTest"
        )
    }

    private fun assertThatTranspiledCodeMatches(sourceFilePath: String,
                                                expectedSourceFilePath: String, clazzname: String) {
        val basePackage = "my.base.pckg"
        val language = App.Language.COBOL

        val sourceFileResource: String = AppTest::class.java.getResource(sourceFilePath).file
        val sourceFile = File(sourceFileResource)
        val outputDir = temporaryFolder.newFolder("cobol-outputdir")

        val generatedSourceFiles = CliRunnerImpl().run(
                sourceFile = sourceFile,
                clazzName = clazzname,
                basePackage = basePackage,
                language = language,
                showGuiTree = false,
                outputDir = outputDir
        )
        assertThat(generatedSourceFiles.isEmpty(), Is(false))

        // get the expectation:
        val expectedSourceFileResource: String = AppTest::class.java.getResource(expectedSourceFilePath).file
        val expectedSourceFile = File(expectedSourceFileResource)
        val expectedLines = expectedSourceFile.readLines()

        // does the generated source file match the expectation?
        generatedSourceFiles
                .first()
                .readLines()
                .forEachIndexed { i, l ->
                    assertThatLinesMatch(i, l, expectedLines)
                }
    }

    private fun assertThatLinesMatch(i: Int, l: String, expectedLines: List<String>) {
        assertThat("Line $i didn't match the expected one!", l, Is(expectedLines[i]))
    }

}
