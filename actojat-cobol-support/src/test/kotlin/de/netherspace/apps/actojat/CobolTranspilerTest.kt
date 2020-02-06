package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl
import org.junit.Ignore
import org.junit.Test
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
     */
    @Test(expected = IllegalStateException::class)
    fun testCobolSourceNotFound() {
        val sourceFile = "/cobol-sources/test-source-thatdoesntexist.cob"
        testSourceNotFound(sourceFile)
    }

    /**
     * Tests, whether the transpiler successfully transpiles a simple Hello-World Cobol program.
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
     */
    @Test
    fun testCobolSimpleLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-simpleloop.cob"
        val clazzName = "SimpleLoop"
        val expectedCode = "package cobol.test.pckg;public class SimpleLoop {public void paragraph_MainProgram(){" +
                "for (int _internal67B28F0=1; _internal67B28F0<=15; _internal67B28F0=(_internal67B28F0+1)) { paragraph_DisplayHelloWorld(); }" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a loop with a (global) variable.
     */
    @Test
    fun testCobolLoopWithIdTranspilation() {
        val sourceFile = "/cobol-sources/test-source-loopwithid.cob"
        val clazzName = "LoopWithId"
        val expectedCode = "package cobol.test.pckg;public class LoopWithId {public short n = 5;public void paragraph_MainProgram(){" +
                "for (int _internal67B28F0=1; _internal67B28F0<=n; _internal67B28F0=(_internal67B28F0+1)) { paragraph_DisplayHelloWorld(); }" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}" +
                "public void paragraph_DoSomethingElse(){System.out.println(\"Something\");System.out.println(\"else!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a loop with an "inline" body.
     */
    @Test
    fun testCobolLoopWithInlineBodyTranspilation() {
        val sourceFile = "/cobol-sources/test-source-loopwithinlinebody.cob"
        val clazzName = "LoopWithInlineBody"
        val expectedCode = "package cobol.test.pckg;public class LoopWithInlineBody {public short MyCounter = 3;" +
                "public void paragraph_MainProgram(){for (int _internal67B28F0=1; _internal67B28F0<=MyCounter; " +
                "_internal67B28F0=(_internal67B28F0+1)) { System.out.println(\"Inline!\"); }System.out.println(\"Done!\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL (i.e. while) loop.
     */
    @Test
    fun testCobolPerformUntilLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-performuntil.cob"
        val clazzName = "PerformUntilOne"
        val expectedCode = "package cobol.test.pckg;public class PerformUntilOne {public int VeryVariable = 1;" +
                "public void paragraph_MainProgram(){while (!(VeryVariable==8)) { paragraph_DisplayHelloWorld(); }" +
                "System.out.println(\"ImDone!\");return;}public void paragraph_DisplayHelloWorld(){System.out." +
                "println(\"Rock\");System.out.println(\"on!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL loop with
     * an explicit BEFORE or AFTER parameter.
     */
    @Test
    fun testCobolPerformUntilLoopWithExplicitBeforeAfterTranspilation() {
        val sourceFile = "/cobol-sources/test-source-prfrmntl-testx.cob"
        val clazzName = "PerformUntilWithTestBeforeAfter"
        val expectedCode = "package cobol.test.pckg;public class PerformUntilWithTestBeforeAfter {public short " +
                "VeryVeryVariable = 1;public void paragraph_MainProgram(){while (!(VeryVeryVariable==2)) { " +
                "paragraph_DisplayImAWhileLoop(); }do { paragraph_DisplayImADoWhileLoop(); } while " +
                "(!(VeryVeryVariable==2));System.out.println(\"Done!\");return;}public void paragraph_DisplayImAWhileLoop(){" +
                "System.out.println(\"whileLoop\");}public void paragraph_DisplayImADoWhileLoop(){System.out." +
                "println(\"doWhileLoop\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL loop with
     * multiple block names as part of a THRU statement.
     */
    @Test
    @Ignore // TODO: this test should work as soon as THRU statements are transpiled properly
    fun testCobolPerformUntilLoopWithThruMultipleBlocknamesTranspilation() {
        val sourceFile = "/cobol-sources/test-source-prfrmntl-multiblx.cob"
        val clazzName = "WhileLoopzWMB"
        val expectedCode = "package cobol.test.pckg;public class WhileLoopzWMB {public int VeryVariable = 1;public " +
                "void paragraph_MainProgram(){while (!(VeryVariable==12)) { paragraph_DisplayOne();paragraph_DisplayTwo(); }" +
                "System.out.println(\"Aaaannnd\");while (!(VeryVariable==8)) { paragraph_DisplayOne();paragraph_DisplayTwo();" +
                "paragraph_DisplayThree(); }System.out.println(\"ImDone!\");return;}public void paragraph_DisplayOne(){" +
                "System.out.println(\"Rock\");}public void paragraph_DisplayTwo(){System.out.println(\"on!\");}" +
                "public void paragraph_DisplayThree(){System.out.println(\"Baby!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL loop with
     * an inline body / inline statements.
     */
    @Test
    fun testCobolPerformUntilLoopWithInlineBodyTranspilation() {
        val sourceFile = "/cobol-sources/test-source-prfrmntl-inlnbdy.cob"
        val clazzName = "PrfrmUntilInlineB"
        val expectedCode = "package cobol.test.pckg;public class PrfrmUntilInlineB {public int MyVar = 1;public void " +
                "paragraph_MainProgram(){while (!(MyVar==13)) { System.out.println(\"Inline!\"); }System.out." +
                "println(\"Done\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM-VARYING loop.
     */
    @Test
    fun testCobolPerformVaryingLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-performvarying.cob"
        val clazzName = "SimpleVaryingLoop"
        val expectedCode = "package cobol.test.pckg;public class SimpleVaryingLoop {public int MyCounter = 1;public " +
                "void paragraph_MainProgram(){for (MyCounter=10; MyCounter<=20; MyCounter=(MyCounter+2)) { " +
                "paragraph_DisplaySomething(); }System.out.println(\"ImDone!\");return;}public void " +
                "paragraph_DisplaySomething(){System.out.println(\"Im\");System.out.println(\"varying\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a simple if-then statement.
     */
    @Test
    fun testCobolSimpleIfThenTranspilation() {
        val sourceFile = "/cobol-sources/test-source-ifthen.cob"
        val clazzName = "SimpleIfThen"
        val expectedCode = "package cobol.test.pckg;public class SimpleIfThen {public int n = 5;public void " +
                "paragraph_MainProgram(){if(n<10){System.out.println(\"Yeah\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles different data declarations.
     */
    @Test
    fun testCobolDataDeclarationTranspilation() {
        val sourceFile = "/cobol-sources/test-source-datadecl.cob"
        val clazzName = "DataDeclarationz"
        val expectedCode = "package cobol.test.pckg;public class DataDeclarationz {public short n = 5;public short m =" +
                " 1234;public void paragraph_MainProgram(){if((n+m)<10){System.out.println(\"Yeah\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a sinmple MOVE...TO.
     */
    @Test
    fun testCobolSimpleMoveAssignmentTranspilation() {
        val sourceFile = "/cobol-sources/test-source-simplemove.cob"
        val clazzName = "SimpleMoooove"
        val expectedCode = "package cobol.test.pckg;public class SimpleMoooove {public short n = 5;public " +
                "short m = 1234;public void paragraph_MainProgram(){m=44;return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing an if-then-else statement.
     */
    @Test
    fun testCobolIfThenElseTranspilation() {
        val sourceFile = "/cobol-sources/test-source-ifthenelse.cob"
        val clazzName = "IfThenElse"
        val expectedCode = "package cobol.test.pckg;public class IfThenElse {public short myVar = 5;public void " +
                "paragraph_MainProgram(){if(myVar<=10){System.out.println(\"Yeah\");} else {System.out" +
                ".println(\"Elzze\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles different statements.
     */
    @Test
    fun testCobolConditionsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-conditions.cob"
        val clazzName = "VariousConditions"
        val expectedCode = "package cobol.test.pckg;public class VariousConditions {public int n = 5;public void " +
                "paragraph_MainProgram(){if(n>10){System.out.println(\"oops\");}if(!(n>20)){System.out." +
                "println(\"correct\");}if(n<10){System.out.println(\"yeah\");}if(!(n<10)){System.out." +
                "println(\"damn\");}if(n>100){System.out.println(\"oops2\");}if(!(n>200)){System.out." +
                "println(\"correct2\");}if(n<110){System.out.println(\"yeah2\");}if(!(n<110)){System.out." +
                "println(\"damn2\");}if(n==5){System.out.println(\"eqqqq\");}if(!(n==5)){System.out." +
                "println(\"noteqqqq\");}if(n==445){System.out.println(\"eqqqq2\");}if(!(n==775)){System.out." +
                "println(\"noteqqqq2\");}if(n>=123){System.out.println(\"goe11111\");}if(n>=1550){System.out." +
                "println(\"gtoet2323\");}if(n<=33){System.out.println(\"lteq33\");}if(n<=77112){System.out." +
                "println(\"ltort774444444\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles complex conditions.
     */
    @Test
    fun testComplexCobolConditionsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-complex-conditions.cob"
        val clazzName = "ComplexConditions"
        val expectedCode = "package cobol.test.pckg;public class ComplexConditions {public int a = 25;public int " +
                "b = 15;public int c = 100;public void paragraph_MainProgram(){if(a>b){System.out." +
                "println(\"great0r\");}if(a<(c/33)){System.out.println(\"oneAE\");}if(a<(b+(c/2))){System.out." +
                "println(\"correct\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles left-recursive conditions
     * (i.e. "IF (c / 33) < a THEN..." and the like).
     */
    @Test
    @Ignore
    fun testLrCobolConditionsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-lrexpressions.cob"
        val clazzName = "LrExpressions"
        val expectedCode = "packrn;}}" // TODO: add the real code...
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                clazzName = clazzName,
                expectedCode = expectedCode
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles an (empty) Cobol section.
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
