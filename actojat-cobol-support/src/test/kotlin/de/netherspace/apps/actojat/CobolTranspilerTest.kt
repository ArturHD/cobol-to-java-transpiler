package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.languages.cobol.CobolSourceTranspilerImpl
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Ignore
import org.junit.Test
import org.slf4j.LoggerFactory
import java.io.InputStream
import java.util.function.Supplier
import org.hamcrest.Matchers.`is` as Is

class CobolTranspilerTest : AbstractTranspilerTest<CobolSourceTranspilerImpl>(
        Supplier { CobolSourceTranspilerImpl() },
        cobolBasePackage
) {

    companion object {
        private val log = LoggerFactory.getLogger(CobolTranspilerTest::class.java)
        private const val cobolBasePackage = "cobol.test.pckg"
    }

    private val internalVariableName01 = "_internal3DD742D"

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
        val expectedCode = "package cobol.test.pckg;public class HelloWorld {public" +
                " void paragraph_DisplayHelloWorld(){System.out.println(\"Hello World!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("HelloWorld" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a simple loop.
     */
    @Test
    fun testCobolSimpleLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-simpleloop.cob"
        val expectedCode = "package cobol.test.pckg;public class SimpleLoop {public void paragraph_MainProgram(){" +
                "for (int $internalVariableName01=1; $internalVariableName01<=15; $internalVariableName01=($internalVariableName01+1))" +
                " { paragraph_DisplayHelloWorld(); }" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("SimpleLoop" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a loop with a (global) variable.
     */
    @Test
    fun testCobolLoopWithIdTranspilation() {

        val sourceFile = "/cobol-sources/test-source-loopwithid.cob"
        val expectedCode = "package cobol.test.pckg;public class LoopWithId {public short n = 5;public void paragraph_MainProgram(){" +
                "for (int $internalVariableName01=1; $internalVariableName01<=n; $internalVariableName01=($internalVariableName01+1)) " +
                "{ paragraph_DisplayHelloWorld(); }" +
                "return;}public void paragraph_DisplayHelloWorld(){System.out.println(\"Hello\");System.out.println(\"World!\");}" +
                "public void paragraph_DoSomethingElse(){System.out.println(\"Something\");System.out.println(\"else!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("LoopWithId" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a loop with an "inline" body.
     */
    @Test
    fun testCobolLoopWithInlineBodyTranspilation() {
        val sourceFile = "/cobol-sources/test-source-loopwithinlinebody.cob"
        val expectedCode = "package cobol.test.pckg;public class LoopWithInlineBody {public short MyCounter = 3;" +
                "public void paragraph_MainProgram(){for (int $internalVariableName01=1; $internalVariableName01<=MyCounter; " +
                "$internalVariableName01=($internalVariableName01+1)) { System.out.println(\"Inline!\"); }System.out." +
                "println(\"Done!\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("LoopWithInlineBody" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL (i.e. while) loop.
     */
    @Test
    fun testCobolPerformUntilLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-performuntil.cob"
        val expectedCode = "package cobol.test.pckg;public class SimpleWhileLoop {public int VeryVariable = 1;" +
                "public void paragraph_MainProgram(){while (!(VeryVariable==8)) { paragraph_DisplayHelloWorld(); }" +
                "System.out.println(\"Im done!\");return;}public void paragraph_DisplayHelloWorld(){System.out." +
                "println(\"Rock\");System.out.println(\"on!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("SimpleWhileLoop" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL loop with
     * an explicit BEFORE or AFTER parameter.
     */
    @Test
    fun testCobolPerformUntilLoopWithExplicitBeforeAfterTranspilation() {
        val sourceFile = "/cobol-sources/test-source-prfrmntl-testx.cob"
        val expectedCode = "package cobol.test.pckg;public class WhileLoopWithTest {public short " +
                "VeryVeryVariable = 1;public void paragraph_MainProgram(){while (!(VeryVeryVariable==2)) { " +
                "paragraph_DisplayImAWhileLoop(); }do { paragraph_DisplayImADoWhileLoop(); } while " +
                "(!(VeryVeryVariable==2));System.out.println(\"Done!\");return;}public void paragraph_DisplayImAWhileLoop(){" +
                "System.out.println(\"whileLoop\");}public void paragraph_DisplayImADoWhileLoop(){System.out." +
                "println(\"doWhileLoop\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("WhileLoopWithTest" to expectedCode)
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
        val expectedCode = "package cobol.test.pckg;public class WhileLoopzWMB {public int VeryVariable = 1;public " +
                "void paragraph_MainProgram(){while (!(VeryVariable==12)) { paragraph_DisplayOne();paragraph_DisplayTwo(); }" +
                "System.out.println(\"Aaaannnd\");while (!(VeryVariable==8)) { paragraph_DisplayOne();paragraph_DisplayTwo();" +
                "paragraph_DisplayThree(); }System.out.println(\"ImDone!\");return;}public void paragraph_DisplayOne(){" +
                "System.out.println(\"Rock\");}public void paragraph_DisplayTwo(){System.out.println(\"on!\");}" +
                "public void paragraph_DisplayThree(){System.out.println(\"Baby!\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("WhileLoopzWMB" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM..UNTIL loop with
     * an inline body / inline statements.
     */
    @Test
    fun testCobolPerformUntilLoopWithInlineBodyTranspilation() {
        val sourceFile = "/cobol-sources/test-source-prfrmntl-inlnbdy.cob"
        val expectedCode = "package cobol.test.pckg;public class PrfrmUntilInlineB {public int MyVar = 1;public void " +
                "paragraph_MainProgram(){while (!(MyVar==13)) { System.out.println(\"Inline!\"); }System.out." +
                "println(\"Done\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("PrfrmUntilInlineB" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a PERFORM-VARYING loop.
     */
    @Test
    fun testCobolPerformVaryingLoopTranspilation() {
        val sourceFile = "/cobol-sources/test-source-performvarying.cob"
        val expectedCode = "package cobol.test.pckg;public class SimpleVaryingLoop {public int MyCounter = 1;public " +
                "void paragraph_MainProgram(){for (MyCounter=10; MyCounter<=20; MyCounter=(MyCounter+2)) { " +
                "paragraph_DisplaySomething(); }System.out.println(\"Im done!\");return;}public void " +
                "paragraph_DisplaySomething(){System.out.println(\"Im\");System.out.println(\"varying\");}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("SimpleVaryingLoop" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing a simple if-then statement.
     */
    @Test
    fun testCobolSimpleIfThenTranspilation() {
        val sourceFile = "/cobol-sources/test-source-ifthen.cob"
        val expectedCode = "package cobol.test.pckg;public class SimpleIfThen {public int n = 5;public void " +
                "paragraph_MainProgram(){if(n<10){System.out.println(\"Yeah\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("SimpleIfThen" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles different data declarations.
     */
    @Test
    fun testCobolDataDeclarationTranspilation() {
        val sourceFile = "/cobol-sources/test-source-datadecl.cob"
        val expectedCode = "package cobol.test.pckg;public class DataDeclarationz {public short n = 5;public short m =" +
                " 1234;public void paragraph_MainProgram(){if((n+m)<10){System.out.println(\"Yeah\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("DataDeclarationz" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles hierarchical data declarations.
     */
    @Test
    fun testHierarchicalDataDeclarationTranspilation() {
        val sourceFile = "/cobol-sources/test-source-hierarchdata.cob"

        // expected code for class "HierarchData":
        val hierarchData = "package cobol.test.pckg;public class HierarchData {public short n = 5;public Complexx " +
                "complexx;public SomewhatComplex somewhatComplex;public Xelpmoc xelpmoc;public short m = 1234;" +
                "public void paragraph_MainProgram(){if((n+m)<10){System.out.println(\"Yeah\");}return;}}"

        // expected code for class "FILENUM":
        val filenum = "package cobol.test.pckg;public class FILENUM {public String ggggg;public String hhhhh;}"

        // expected code for class "SomewhatComplex":
        val somewhatComplex = "package cobol.test.pckg;public class SomewhatComplex {public String kkkkk = \"xy\";public String lllll;}"

        // expected code for class "MoreComplexx":
        val moreComplexx = "package cobol.test.pckg;public class MoreComplexx {public String ddddd;public " +
                "String eeeee;public String fffff;public FILENUM fILENUM;public int iiiii;public int jjjjj;}"

        // expected code for class "Xelpmoc":
        val xelpmoc = "package cobol.test.pckg;public class Xelpmoc {public String mmmmm;public String nnnnn;}"

        // expected code for class "Complexx":
        val complexx = "package cobol.test.pckg;public class Complexx {public String aaaaa;public String bbbbb;" +
                "public String ccccc;public MoreComplexx moreComplexx;}"

        val expectations = mapOf(
                "HierarchData" to hierarchData,
                "FILENUM" to filenum,
                "SomewhatComplex" to somewhatComplex,
                "MoreComplexx" to moreComplexx,
                "Xelpmoc" to xelpmoc,
                "Complexx" to complexx
        )
        val generatedClazzes = doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = expectations
        )
        assertThat(generatedClazzes.size, Is(6))
    }

    /**
     * Tests, whether the transpiler successfully transpiles a sinmple MOVE...TO.
     */
    @Test
    fun testCobolSimpleMoveAssignmentTranspilation() {
        val sourceFile = "/cobol-sources/test-source-simplemove.cob"
        val expectedCode = "package cobol.test.pckg;public class SimpleMove {public short n = 5;public " +
                "short m = 1234;public void paragraph_MainProgram(){m=44;return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("SimpleMove" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles MOVEs with alphanumeric values.
     */
    @Test
    fun testCobolAlphanumMoveAssignmentTranspilation() {
        val sourceFile = "/cobol-sources/test-source-alphanummv.cob"
        val expectedCode = "package cobol.test.pckg;public class AlphaNumMv {public String Surname = \"Chuck \";public " +
                "String TruncateName = \"ab\";public String FillName = \"123456789012\";public void paragraph_" +
                "MainProgram(){Surname=\"Arnold\";TruncateName=\"Brnold\";FillName=\"Crnold\";return;}}"
        // TODO: This expected code does NOT honor COBOL's truncate/fillUp semantics when moving different-sized
        // TODO: alphanumeric values! See the CobolVisitor class for details!
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("AlphaNumMv" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles comments.
     */
    @Test
    fun testCobolCommentsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-commentz.cob"
        val expectedCode = "package cobol.test.pckg;public class Commentz {public short MyCounter = 3;" +
                "public void paragraph_MainProgram(){for (int $internalVariableName01=1; $internalVariableName01<=MyCounter; " +
                "$internalVariableName01=($internalVariableName01+1)) { System.out.println(\"Inline!\"); }System.out." +
                "println(\"Done!\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("Commentz" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles multi-line comments.
     */
    @Test
    fun testCobolMultiLineCommentsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-multilcommntz.cob"
        val expectedCode = "package cobol.test.pckg;public class Multilcommntz {public short MyCounter = 3;" +
                "public void paragraph_MainProgram(){for (int $internalVariableName01=1; $internalVariableName01<=MyCounter; " +
                "$internalVariableName01=($internalVariableName01+1)) { System.out.println(\"Inline!\"); }System.out." +
                "println(\"Done!\");return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("Multilcommntz" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles a program containing an if-then-else statement.
     */
    @Test
    fun testCobolIfThenElseTranspilation() {
        val sourceFile = "/cobol-sources/test-source-ifthenelse.cob"
        val expectedCode = "package cobol.test.pckg;public class IfThenElse {public short myVar = 5;public void " +
                "paragraph_MainProgram(){if(myVar<=10){System.out.println(\"Yeah\");} else {System.out" +
                ".println(\"Elzze\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("IfThenElse" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles different statements.
     */
    @Test
    fun testCobolConditionsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-conditions.cob"
        val expectedCode = "package cobol.test.pckg;public class Conditions {public int n = 5;public void " +
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
                mainClazzName = null,
                expectations = mapOf("Conditions" to expectedCode)
        )
    }

    /**
     * Tests, whether the transpiler successfully transpiles complex conditions.
     */
    @Test
    fun testComplexCobolConditionsTranspilation() {
        val sourceFile = "/cobol-sources/test-source-complex-conditions.cob"
        val expectedCode = "package cobol.test.pckg;public class ComplexConditions {public int a = 25;public int " +
                "b = 15;public int c = 100;public void paragraph_MainProgram(){if(a>b){System.out." +
                "println(\"great0r\");}if(a<(c/33)){System.out.println(\"oneAE\");}if(a<(b+(c/2))){System.out." +
                "println(\"correct\");}return;}}"
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("ComplexConditions" to expectedCode)
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
        val expectedCode = "packrn;}}" // TODO: add the real code...
        doTranspilationTest(
                source = loadSourceFile(sourceFile),
                mainClazzName = null,
                expectations = mapOf("LrConditions" to expectedCode)
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
