package de.netherspace.apps.actojat;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.io.InputStream;
import java.util.function.Supplier;

import de.netherspace.apps.actojat.SourceTranspiler;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import org.antlr.v4.runtime.tree.ParseTree;


/**
 * An abstract test class.
 */
public abstract class AbstractTranspilerTest<T extends SourceTranspiler> {

    private Supplier<SourceTranspiler> constructorExpr;
    private String testBasePackage;


    /**
     * The constructor.
     *
     * @param constructorExpr A lambda expression to create a new subclass instance
     * @param testBasePackage The base package that is used during transpilation
     */
    public AbstractTranspilerTest(Supplier<SourceTranspiler> constructorExpr, String testBasePackage) {
        super();
        this.constructorExpr = constructorExpr;
        this.testBasePackage = testBasePackage;
    }


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
    protected void doTranspilationTest(String sourceFile, String clazzName, String expectedCode)
            throws ParserException, SourceGenerationException, IOException, IntermediateRepresentationException {
        SourceTranspiler transpiler = constructorExpr.get();
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream(sourceFile);
        ParseTree parseTree = transpiler.parseInputStream(inputStream);
        assertNotNull(parseTree);

        JavaLanguageConstruct ir = transpiler.generateIntermediateJavaRepresentation(parseTree);
        assertNotNull(ir);

        String actualCode = transpiler.generateSourceCode(ir, clazzName, testBasePackage);

        System.out.println(actualCode); //TODO: write the formatted code instead!
        assertEquals(expectedCode, actualCode);
    }
}
