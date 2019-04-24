package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.ir.java.Assignment;
import de.netherspace.apps.actojat.ir.java.BasicConstruct;
import de.netherspace.apps.actojat.ir.java.Expression;
import de.netherspace.apps.actojat.ir.java.ForLoop;
import de.netherspace.apps.actojat.ir.java.FunctionCall;
import de.netherspace.apps.actojat.ir.java.Import;
import de.netherspace.apps.actojat.ir.java.JavaConstructType;
import de.netherspace.apps.actojat.ir.java.Method;
import de.netherspace.apps.actojat.ir.java.Program;
import de.netherspace.apps.actojat.ir.java.Statement;
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

  private Map<String, Pair<BasicConstruct, JavaConstructType>> systemFunctions;
  private Map<String, Method> sourceMethodNamesToJavaMethods;

  /**
   * The default constructor.
   */
  JavaIrToSourceCodeTranslator(Map<String, Pair<BasicConstruct,
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
    log.trace("statementToCode() ...");
    log.trace("Statement = " + stmnt);

    // is the statement an assignment?
    if (stmnt instanceof Assignment) {
      log.trace("The statement is an assignment!");
      final Assignment assignment = (Assignment) stmnt;
      if (assignment.getLhs().getType() != null) {
        return assignment.getLhs().getType()
            + " " + assignment.getLhs().getVariableName()
            + "=" + assignment.getRhs() + ";";
      } else {
        return assignment.getLhs().getVariableName() + "=" + assignment.getRhs() + ";";
      }
      //TODO: nested expressions!


      // is the statement a for-loop?
    } else if (stmnt instanceof ForLoop) {
      log.trace("The statement is a foor-loop!");
      final ForLoop loop = (ForLoop) stmnt;
      return this.forLoopToCode.apply(loop);


      // is the statement a function call?
    } else if (stmnt instanceof FunctionCall) {
      log.trace("The statement is a function call!");
      final FunctionCall functionCall = (FunctionCall) stmnt;
      return this.functioncallToCode.apply(functionCall);


      // the statement is neither an assignment nor a function call nor a loop, nor a ...:
    } else {
      log.error("Couldn't match statement!");
      return null; //ooops...
    }
  };


  /**
   * Maps a for-loop to its corresponding code snippet.
   */
  private final Function<ForLoop, String> forLoopToCode = loop -> {
    final String loopVariable;
    if (loop.getLoopVariable().getLhs() != null) {
      //loopVariable = loop.getLoopVariable().getLhs() + " " + loop.getLoopVariable().getRhs();
      loopVariable = this.statementToCode.apply(loop.getLoopVariable());
      // TODO: map the loop variable as an actual assignment (to get the "=" etc.)!
    } else {
      loopVariable = loop.getLoopVariable().getRhs();
    }

    final String loopHeader = "for (" + loopVariable
        + ", " + loop.getLoopCondition()
        + ", " + loop.getLoopIncrement()
        + ")";

    final String loopBody = Arrays
        .stream(loop.getBody())
        .map(this.statementToCode)
        .reduce("", this.stringAccumulator);

    return loopHeader + " { " + loopBody + " };";
  };


  /**
   * Maps a function call to its corresponding code snippet.
   */
  private final Function<FunctionCall, String> functioncallToCode = functionCall -> {
    // extract its parameters (e.g. the "Hello!" in DISPLAY("Hello!")):
    log.trace("There are " + functionCall.getParameters().size() + " parameters...");
    final String parameters = functionCall
        .getParameters()
        .stream()
        .map(Expression::getParts) // TODO: correct mapping...
        .flatMap(a -> Arrays.stream(a))
        //.map(param -> param + ", ")
        .reduce("", this.stringAccumulator);
    log.trace("The function's parameters are: '" + parameters + "'");

    // check, whether this source statement maps canonically to a
    // pre-defined Java method (e.g. "DISPLAY" -> "System.out.println"):
    final String functionName;
    if (!this.systemFunctions.containsKey(functionCall.getName())) {
      // no, therefore we'll take its original name:
      final String functionCallName = functionCall.getName();
      log.trace("The function's name is: '" + functionCallName + "'");
      final Method m = this.sourceMethodNamesToJavaMethods.get(functionCallName);
      functionName = m.getName();

    } else {
      // yes, there is a corresponding Java method!
      final Pair<BasicConstruct, JavaConstructType> systemFunction = this
          .systemFunctions
          .get(functionCall.getName());
      final BasicConstruct f = systemFunction.getFirst();
      final JavaConstructType constructType = systemFunction.getSecond();

      // check, if its a _method_ or a mere _keyword_ (e.g. "return"):
      if (constructType == JavaConstructType.KEYWORD) {
        final String fc = f.getRawName() + ";";
        log.trace("The transpiled function call is: '" + fc + "'");
        return fc;
        // TODO: if its a "return" WITH an associated value (e.g. "return 0" in C), then
        // TODO: we have to map to "System.exit(value)" iff it's a main method!
      } else {
        functionName = f.getRawName();
      }
    }
    final String fc = functionName + "(" + parameters + ")" + ";";
    log.trace("The transpiled function call is: '" + fc + "'");
    return fc;
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
          .map(this.statementToCode)
          .reduce("", this.stringAccumulator);
    }

    return signature + "{" + body + "}";
  };

}
