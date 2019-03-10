package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.intermediaterepresentation.java.Assignment;
import de.netherspace.apps.actojat.intermediaterepresentation.java.BasicFunction;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Expression;
import de.netherspace.apps.actojat.intermediaterepresentation.java.ForLoop;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaConstructType;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Statement;
import de.netherspace.apps.actojat.util.Pair;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * A translator that generates Java code from an intermediate representation.
 */
@Slf4j
public class JavaIrToSourceCodeTranslator {

  private StringBuilder builder;

  @Getter
  @Setter
  private String className;

  @Getter
  @Setter
  private String basePackage;

  private Map<String, Pair<BasicFunction, JavaConstructType>> systemFunctions;
  private Map<String, Method> sourceMethodNamesToJavaMethods;

  /**
   * The default constructor.
   */
  JavaIrToSourceCodeTranslator(Map<String, Pair<BasicFunction,
      JavaConstructType>> systemFunctions) {
    super();
    this.builder = new StringBuilder();
    this.systemFunctions = systemFunctions;
    this.sourceMethodNamesToJavaMethods = new HashMap<>();
  }


  /**
   * Generates a piece of Java code for a given intermediate representation.
   *
   * @param program The IR
   * @return The generated piece of code
   * @throws SourceGenerationException If className or basePackage are not set
   */
  public synchronized String generateCodeFromIr(Program program) throws SourceGenerationException {
    if (className == null || className.length() == 0
        || basePackage == null || basePackage.length() == 0) {
      throw new SourceGenerationException();
    }

    append.accept("package " + this.basePackage + ";");

    program.getImports().stream()
        .map(irImportToCode)
        .forEach(append);

    append.accept("public class " + this.className + " {");

    // store the sourceMethodName -> JavaIrMethod mapping as a member:
    this.sourceMethodNamesToJavaMethods = program.getMethods();

    program.getMethods()
        .values()
        .stream()
        .map(irMethodToCode)
        .forEach(append);

    append.accept("}");
    return builder.toString();
  }


  /**
   * Appends a string to the instance's string builder.
   */
  private final Consumer<String> append = s -> builder.append(s);


  /**
   * Accumulates a string.
   */
  private final BinaryOperator<String> stringAccumulator
      = (accumulatedBody, stmnt) -> accumulatedBody + stmnt;


  /**
   * Maps an IR import to its corresponding code snippet.
   */
  private final Function<Import, String> irImportToCode = i -> {
    return "import " + this.basePackage + "." + i.getName() + ";";
  };


  /**
   * Maps an IR statement to its corresponding code snippet.
   */
  private final Function<Statement, String> statementToCode = stmnt -> {
    // is the statement an assignment?
    if (stmnt instanceof Assignment) {
      final Assignment assignment = (Assignment) stmnt;
      return assignment.getLhs() + "=" + assignment.getRhs() + ";";
      //TODO: composite expressions!


      // is the statement a for-loop?
    } else if (stmnt instanceof ForLoop) {
      final ForLoop loop = (ForLoop) stmnt;
      final String loopHeader = "for (" + loop.getLoopCounter() + ")";

      final String loopBody = Arrays
          .stream(loop.getBody())
          .map(this.statementToCode)
          .reduce("", this.stringAccumulator);

      return loopHeader + " { " + loopBody + " };";


      // is the statement a function call?
    } else if (stmnt instanceof FunctionCall) {
      final FunctionCall functionCall = (FunctionCall) stmnt;

      // extract its parameters (e.g. the "Hello!" in DISPLAY("Hello!")):
      final String parameters = functionCall
          .getParameters()
          .stream()
          .map(Expression::getParts) // TODO: correct mapping...
          .flatMap(a -> Arrays.stream(a))
          //.map(param -> param + ", ")
          .reduce("", this.stringAccumulator);

      // check, whether this source statement maps canonically to a
      // pre-defined Java method (e.g. "DISPLAY" -> "System.out.println"):
      final String functionName;
      if (!this.systemFunctions.containsKey(functionCall.getName())) {
        // no, therefore we'll take its original name:
        final Method m = this.sourceMethodNamesToJavaMethods.get(functionCall.getName());
        functionName = m.getName();

      } else {
        // yes, there is a corresponding Java method!
        final Pair<BasicFunction, JavaConstructType> systemFunction = this
            .systemFunctions
            .get(functionCall.getName());
        final BasicFunction f = systemFunction.getFirst();
        final JavaConstructType constructType = systemFunction.getSecond();

        // check, if its a _method_ or a mere _keyword_ (e.g. "return"):
        if (constructType == JavaConstructType.KEYWORD) {
          return f.getRawName() + ";";
        } else {
          functionName = f.getRawName();
        }
      }
      return functionName + "(" + parameters + ")" + ";";


      // the statement is neither an assignment nor a function call nor a loop, nor a ...:
    } else {
      return ""; //ooops...
    }
  };


  /**
   * Maps an IR method to its corresponding code snippet.
   */
  private final Function<Method, String> irMethodToCode = m -> {
    final String defaultAccessModifier = "public";
    final String defaultReturnType = "void";

    final String signature = defaultAccessModifier + " "
                              + defaultReturnType + " "
                              + m.getName() + "()";
    final String body;
    if (m.getStatements().isEmpty()) {
      body = "";
    } else {
      body = m.getStatements()
          .stream()
          .map(statementToCode)
          .reduce("", stringAccumulator);
    }

    return signature + "{" + body + "}";
  };

}
