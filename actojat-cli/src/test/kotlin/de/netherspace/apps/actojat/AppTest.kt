package de.netherspace.apps.actojat

import org.hamcrest.MatcherAssert.assertThat
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import java.io.File
import org.hamcrest.Matchers.`is` as Is

class AppTest {

    @get:Rule
    public val temporaryFolder = TemporaryFolder() // TODO: creates a temp. folder under /tmp/ -> should rather be under /target!

    @Test
    fun testTranspileSingleFile() {
        val sourceFilePath = "/cobol-sources/test-source-hellocobol.cob"
        val sourceFileResource: String = AppTest::class.java.getResource(sourceFilePath).file
        val sourceFile = File(sourceFileResource)
        val outputDir = temporaryFolder.newFolder("cobol-outputdir1")

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

    // TODO: test the transpilation of all sources within a directory!

}
