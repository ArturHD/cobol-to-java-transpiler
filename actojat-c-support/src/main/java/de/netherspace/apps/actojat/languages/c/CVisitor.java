package de.netherspace.apps.actojat.languages.c;

import de.netherspace.apps.actojat.c_grammarBaseVisitor;
import de.netherspace.apps.actojat.c_grammarParser;
import de.netherspace.apps.actojat.ir.java.Assignment;
import de.netherspace.apps.actojat.ir.java.Expression;
import de.netherspace.apps.actojat.ir.java.ForLoop;
import de.netherspace.apps.actojat.ir.java.FunctionCall;
import de.netherspace.apps.actojat.ir.java.Import;
import de.netherspace.apps.actojat.ir.java.IrFactory;
import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.ir.java.LeftHandSide;
import de.netherspace.apps.actojat.ir.java.Method;
import de.netherspace.apps.actojat.ir.java.Program;
import de.netherspace.apps.actojat.ir.java.Statement;
import de.netherspace.apps.actojat.languages.BaseVisitor;
import de.netherspace.apps.actojat.util.Pair;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CVisitor extends c_grammarBaseVisitor<JavaLanguageConstruct> implements BaseVisitor {

  private final IrFactory irFactory = new IrFactory();

  private Program javaProgram;

  /**
   * The default constructor.
   */
  CVisitor() {
    super();
    this.javaProgram = irFactory.createProgram();
  }


  @Override
  public JavaLanguageConstruct visit(ParseTree tree) {
    super.visit(tree);
    return javaProgram;
  }


  /**
   * Maps a list of C arguments to a HashMap.
   */
  Function<c_grammarParser.ArgumentlistContext, Map<String, String>> argumentsToJavaArgs = args -> {
    //TODO!
    return null;
  };


  /**
   * Maps a C assignment operator to a Java assignm. op.
   */
  Function<TerminalNode, String> assignmentOperatorToJavaOperator = op -> {
    return op.getText();
  };


  @Override
  public JavaLanguageConstruct visitFunctiondeclr(c_grammarParser.FunctiondeclrContext ctx) {
    final String methodName = computeMethodName(ctx);
    final Method javaMethod = irFactory.createMethod(methodName);

    /*List<Argument> jarguments = argumentsToJavaArgs.apply(ctx.argumentlist())
                             .entrySet()
                             .stream()
                             .map(argEntryToJavaArgument)
                             .collect(Collectors.toList());
    javaMethod.getArguments().addAll(jarguments);*/

    final List<Statement> jstatements = ctx.block()
        .expressionlist()
        .expression()
        .stream()
        .map(expressionToJavaStatement)
        .collect(Collectors.toList());
    javaMethod.getStatements().addAll(jstatements);

    javaProgram.getMethods().put(methodName, javaMethod);
    return javaMethod;
  }


  /**
   * Maps a C function to a Java method's name.
   */
  private String computeMethodName(c_grammarParser.FunctiondeclrContext ctx) {
    return ctx.ID().getText();
  }


  /**
   * Maps a C parameter (e.g. the "HelloWorld" in
   * printf("HelloWorld")
   * ) to a Java expression.
   */
  private final Function<c_grammarParser.ArgumentContext, Expression> parameterToJavaExpression
      = param -> {
        final String[] parts = {param.getText()};
        return irFactory.createExpression(parts);
      };


  /**
   * Maps a C expression to a Java statement.
   */
  private Function<c_grammarParser.ExpressionContext, Statement> expressionToJavaStatement = ex -> {
    // TODO: distinguish whether its a function call via the grammar!
    if (ex.functioncall() != null) {
      // set the function's name (e.g. 'doSomething' for 'bla = doSomething();' ):
      final String functionName = ex.functioncall().ID().getText();
      final FunctionCall functionCall = irFactory.createFunctionCall(functionName);

      if (ex.functioncall().argumentlist() != null) {
        final List<Expression> parameters = ex.functioncall()
            .argumentlist()
            .argument()
            .stream()
            .map(this.parameterToJavaExpression)
            .collect(Collectors.toList());
        functionCall.getParameters().addAll(parameters);
      }
      return functionCall;
    }

    // its a mere assignment:
    if (ex.assignment() != null) {
      return this.assignmentToJavaAssignment.apply(ex.assignment());
    }

    if (ex.returnstatement() != null) {
      final String functionName = "return"; // TODO: create an enum holding these values!
      final FunctionCall functionCall = irFactory.createFunctionCall(functionName);
      // ...
      return functionCall;
    }

    if (ex.forloop() != null) {
      final c_grammarParser.ForloopContext forLoop = ex.forloop();
      return this.forLoopToJavaForLoop.apply(forLoop);
    }

    System.err.println("couldn't determine statement type:");
    System.err.println(" " + ex.getText());
    return null;
  };


  /**
   * Maps a C for-loop to a Java for-loop.
   */
  private Function<c_grammarParser.ForloopContext, ForLoop> forLoopToJavaForLoop = fl -> {
    Assignment loopVariable = null;
    if (fl.assignment() != null) {
      // Case 1: "for (int i=0, ..."
      loopVariable = this.assignmentToJavaAssignment.apply(fl.assignment());

    } else if (fl.rhs() != null) {
      // Case 2: "for (i=0, ..."
      final String rhs = fl.rhs().getText();
      //loopVariable.setRhs(rhs); // TODO: does it work just like "fl.assignment()"?
      loopVariable = this.assignmentToJavaAssignment.apply(fl.assignment());

    } else {
      System.err.println("No loop variable present!");
      return null;
    }

    String loopCondition = fl.condition().getText();
    String loopIncrement = fl.incrementstatement().getText();

    final Statement[] body = fl
        .block()
        .expressionlist()
        .expression()
        .stream()
        .map(expressionToJavaStatement)
        .toArray(Statement[]::new);

    return irFactory.createForLoop(loopVariable, loopCondition, loopIncrement, body);
  };


  /**
   * Maps a C assignment to a Java assignment.
   */
  private Function<c_grammarParser.AssignmentContext, Assignment> assignmentToJavaAssignment
      = assignment -> {
        final Pair<String, String> lhs = this.computeLeftHandSide(assignment.lhs());
        final String lhsTypeAnnotation = lhs.getFirst();
        final String lhsVariableName = lhs.getSecond();
        final LeftHandSide jlhs = irFactory.createLeftHandSide(lhsTypeAnnotation, lhsVariableName);
        final String jrhs = this.computeRightHandSide(assignment.rhs());
        return irFactory.createAssignment(jlhs, jrhs);
      };


  /**
   * Maps a left-hand side identifier to a Java identifier: type x name.
   */
  private Pair<String, String> computeLeftHandSide(c_grammarParser.LhsContext lhs) {
    if (lhs.variabledecl() != null) {
      final String typeAnnotation = lhs.variabledecl().primitivetype().getText();
      final String variableName = lhs.variabledecl().ID().getText();
      return new Pair<>(typeAnnotation, variableName);

    } else {
      final String variableName = lhs.ID().getText();
      return new Pair<>(null, variableName);
    }
  }


  /**
   * Maps a right-hand side to a Java RHS.
   */
  private String computeRightHandSide(c_grammarParser.RhsContext rhs) {
    return rhs.getText(); // TODO: recursively handle nested expressions!
  }


  @Override
  public JavaLanguageConstruct visitImportheader(c_grammarParser.ImportheaderContext ctx) {
    final String importName = computeImportName(ctx);
    final Import jimport = irFactory.createImport(importName);
    javaProgram.getImports().add(jimport);
    return jimport;
  }


  /**
   * Maps a C include to a Java's import file name.
   */
  private String computeImportName(c_grammarParser.ImportheaderContext include) {
    final String includeWithoutSeparatingDot = include.FILEID()
        .getText().replaceAll("\\.", "_");
    final String includeWithoutSlashes = includeWithoutSeparatingDot
        .replaceAll("/", "_");
    final String includeWithoutDashes = includeWithoutSlashes
        .replaceAll("-", "_");
    return includeWithoutDashes;
    // TODO: blacklist standard libs (e.g. "<stdio.h>")!
  }

}
