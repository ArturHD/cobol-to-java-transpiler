package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.intermediaterepresentation.java.Assignment;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Statement;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * A translator that generates Java code from an intermediate representation.
 */
public class JavaIrToSourceCodeTranslator {

  private StringBuilder builder;

  @Getter
  @Setter
  private String className;

  @Getter
  @Setter
  private String basePackage;

  private Map<String, String> systemFunctions;

  /**
   * The default constructor.
   */
  public JavaIrToSourceCodeTranslator() {
    super();
    this.builder = new StringBuilder();
    this.systemFunctions = new HashMap<>();

    this.systemFunctions.put("printf", "System.out.println"); //TODO: load from file!
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

    program.getMethods().stream()
        .map(irMethodToCode)
        .forEach(append);

    append.accept("}");

    return builder.toString();
  }


  /**
   * Appends a string to the instance's string builder.
   */
  Consumer<String> append = s -> builder.append(s);


  /**
   * Accumulates a string.
   */
  BinaryOperator<String> stringAccumulator = (accumulatedBody, stmnt) -> accumulatedBody + stmnt;


  /**
   * Maps an IR import to its corresponding code snippet.
   */
  Function<Import, String> irImportToCode = i -> {
    return "import " + this.basePackage + "." + i.getName() + ";";
  };


  /**
   * Maps an IR statement to its corresponding code snippet.
   */
  Function<Statement, String> statementToCode = stmnt -> {
    if (stmnt instanceof Assignment) {
      Assignment assignment = (Assignment) stmnt;
      return assignment.getLhs() + "=" + assignment.getRhs() + ";";
      //TODO: composite expressions!

    } else if (stmnt instanceof FunctionCall) {
      FunctionCall functionCall = (FunctionCall) stmnt;
      String parameters = functionCall.getParameters()
          .stream()
          .reduce("", stringAccumulator);//TODO: correct mapping...
      String functionName = systemFunctions.getOrDefault(functionCall.getName(),
          functionCall.getName());
      return functionName + "(" + parameters + ")" + ";";
    } else {
      return ""; //ooops...
    }
  };


  /**
   * Maps an IR method to its corresponding code snippet.
   */
  Function<Method, String> irMethodToCode = m -> {
    String defaultAccessModifier = "public";
    String defaultReturnType = "void";

    String signature = defaultAccessModifier + " " + defaultReturnType + " " + m.getName() + "()";
    String body = null;
    if (m.getStatements().isEmpty()) {
      body = "";
    } else {
      body = m.getStatements().stream()
          .map(statementToCode)
          .reduce("", stringAccumulator);
    }

    return signature + "{" + body + "}";
  };

}
