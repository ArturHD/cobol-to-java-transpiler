package de.netherspace.apps.actojat;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import de.netherspace.apps.actojat.ir.java.Assignment;
import de.netherspace.apps.actojat.ir.java.BasicConstruct;
import de.netherspace.apps.actojat.ir.java.Expression;
import de.netherspace.apps.actojat.ir.java.FunctionCall;
import de.netherspace.apps.actojat.ir.java.IrFactory;
import de.netherspace.apps.actojat.ir.java.JavaConstructType;
import de.netherspace.apps.actojat.ir.java.LeftHandSide;
import de.netherspace.apps.actojat.ir.java.Method;
import de.netherspace.apps.actojat.ir.java.Program;
import de.netherspace.apps.actojat.util.Pair;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.extern.slf4j.Slf4j;
import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

@Slf4j
public class IrTranspilationTest {

  private static final String testBasePackage = "actojat.ir.test.pckg";
  private final IrFactory irFactory = new IrFactory();


  @Test
  public void testSimpleFunctionCallTranspilation() throws SourceGenerationException {
    final Program program = newProgram();

    final String methodName = "myMethod";
    final Method method = newMethod(methodName);
    program.getMethods().put(methodName, method);

    // this statement appears inside of "myMethod" and calls "doSomething":
    final FunctionCall statement1 = irFactory.createFunctionCall("doSomethingElse");
    assertThat(method.getStatements(), is(not(nullValue())));
    method.getStatements().add(statement1);

    final String methodName2 = "doSomethingElse";
    final Method method2 = irFactory.createMethod(methodName2);
    program.getMethods().put(methodName2, method2);

    assertThat(program.getMethods().containsKey("myMethod"), is(true));
    assertThat(program.getMethods().containsKey("doSomethingElse"), is(true));

    final String expectedCode = "package actojat.ir.test.pckg;public class TestProgram1 {"
        + "public void myMethod(){doSomethingElse();}public void doSomethingElse(){}}";
    doTranspilationTest(program, "TestProgram1", expectedCode);
  }


  @Test
  public void testSimpleAssignmentTranspilation() throws SourceGenerationException {
    final Program program = newProgram();

    final String methodName = "myTestMethod";
    final Method testMethod = newMethod(methodName);
    program.getMethods().put(methodName, testMethod);

    final String type = "int";
    final String variableName = "j";
    final LeftHandSide lhs = irFactory.createLeftHandSide(type, variableName);
    final String rhs = "0";
    final Assignment assignment1 = irFactory.createAssignment(lhs, rhs);
    testMethod.getStatements().add(assignment1);

    final String expectedCode = "package actojat.ir.test.pckg;"
        + "public class SimpleAssignment1 {public void myTestMethod(){int j=0;}}";
    doTranspilationTest(program, "SimpleAssignment1", expectedCode);
  }


  @Test
  public void testHelloWorldTranspilation() throws SourceGenerationException {
    final Program program = newProgram();

    final String methodName = "helloWorld";
    final Method method = newMethod(methodName);
    program.getMethods().put(methodName, method);

    // this statement appears inside of "helloWorld" and prints "HelloWorld":
    final FunctionCall statement1 = irFactory.createFunctionCall("Print");
    final String[] parts1 = {"\"HelloWorld\""};
    statement1.getParameters().add(irFactory.createExpression(parts1));
    assertThat(method.getStatements(), is(not(nullValue())));
    method.getStatements().add(statement1);

    assertThat(program.getMethods().containsKey("helloWorld"), is(true));

    final String expectedCode = "package actojat.ir.test.pckg;public class HelloWorld {"
        + "public void helloWorld(){System.out.print(\"HelloWorld\");}}";
    doTranspilationTest(program, "HelloWorld", expectedCode);
  }


  @Test
  @Ignore
  public void testForLoopTranspilation() {
    // TODO: !!!
    final boolean b = true;
    assertThat(b, is(true));
  }


  /**
   * Some (arbitrary) "system functions".
   *
   * @return a map containing the functions and their IR enum value.
   */
  private HashMap<String, Pair<BasicConstruct, JavaConstructType>> systemFunctions() {
    HashMap<String, Pair<BasicConstruct, JavaConstructType>> map = new HashMap<>();
    map.put("Print", new Pair<>(BasicConstruct.PRINT, JavaConstructType.FUNCTION));
    map.put("Return", new Pair<>(BasicConstruct.RETURN, JavaConstructType.KEYWORD));
    return map;
  }


  /**
   * Performs the actual transpilation test.
   *
   * @param program      The intermediate representation
   * @param clazzName    The program's name
   * @param expectedCode The expected source code after transpilation
   * @throws SourceGenerationException If a source code generation exception occurs
   */
  private void doTranspilationTest(Program program, String clazzName, String expectedCode)
      throws SourceGenerationException {
    final HashMap<String, Pair<BasicConstruct, JavaConstructType>> sysFunctions = systemFunctions();
    final String code = generateCode(program, clazzName, testBasePackage, sysFunctions);
    assertThat(code, is(not(nullValue())));
    log.debug(code);
    assertThat(code, is(expectedCode));
  }


  /**
   * Creates actual source code from an intermediate representation (and adds a package
   *
   * @param program         The intermediate representation
   * @param clazzName       The program's name
   * @param basePackage     The desired Java base package
   * @param systemFunctions A map of system functions to canonical Java functions (e.g. "printf")
   * @return A single piece of source code
   * @throws SourceGenerationException If a source code generation exception occurs
   */
  private String generateCode(Program program, String clazzName, String basePackage,
                              Map<String, Pair<BasicConstruct, JavaConstructType>> systemFunctions)
      throws SourceGenerationException {
    final JavaIrToSourceCodeTranslator irTranslator
        = new JavaIrToSourceCodeTranslator(systemFunctions);
    irTranslator.setClassName(clazzName);
    irTranslator.setBasePackage(basePackage);
    return irTranslator.generateCodeFromIr(program);
  }


  /**
   * Creates a new IR program. Checks, whether the program object's underlying
   * collections (imports and methods) are non-null after creation.
   *
   * @return the newly created program
   */
  private Program newProgram() {
    final Program program = irFactory.createProgram();
    assertThat(program.getMethods(), is(not(nullValue())));
    assertThat(program.getImports(), is(not(nullValue())));
    return program;
  }


  /**
   * Creates a new IR method for a given method name. Checks, whether the method
   * object's underlying collections (for arguments and statements) are non-null
   * after creation.
   *
   * @param methodName the method's name
   * @return the newly created method
   */
  private Method newMethod(String methodName) {
    final Method testMethod = irFactory.createMethod(methodName);
    assertThat(testMethod.getStatements(), is(not(nullValue())));
    assertThat(testMethod.getArguments(), is(not(nullValue())));
    return testMethod;
  }

}
