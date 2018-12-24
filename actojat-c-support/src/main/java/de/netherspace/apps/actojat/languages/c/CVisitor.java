package de.netherspace.apps.actojat.languages.c;

import de.netherspace.apps.actojat.c_grammarBaseVisitor;
import de.netherspace.apps.actojat.c_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Argument;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Assignment;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Statement;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CVisitor extends c_grammarBaseVisitor<JavaLanguageConstruct> {

  private Program javaProgram;


  /**
   * The default constructor.
   */
  CVisitor() {
    super();
    this.javaProgram = new Program();
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
   * Maps a C argument (consisting of a type and a name) to a Java argument.
   */
  Function<Entry<String, String>, Argument> argEntryToJavaArgument = e -> {
    Argument jargument = new Argument();
    jargument.setName(e.getKey());
    jargument.setType(e.getValue());
    return jargument;
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
    final Method javaMethod = new Method(methodName);

    /*List<Argument> jarguments = argumentsToJavaArgs.apply(ctx.argumentlist())
                             .entrySet()
                             .stream()
                             .map(argEntryToJavaArgument)
                             .collect(Collectors.toList());
    //TODO: keep order!
    javaMethod.getArguments().addAll(jarguments);*/

    final List<Statement> jstatements = ctx.block()
        .expressionlist()
        .expression()
        .stream()
        .map(expressionToJavaStatement)
        .collect(Collectors.toList());
    //TODO: keep order!
    javaMethod.getStatements().addAll(jstatements);

    javaProgram.getMethods().add(javaMethod);
    return javaMethod;
  }


  /**
   * Maps a C function to a Java method's name.
   */
  private String computeMethodName(c_grammarParser.FunctiondeclrContext ctx) {
    return ctx.ID().getText();
  }


  /**
   * Maps a C expression to a Java statement.
   */
  private Function<c_grammarParser.ExpressionContext, Statement> expressionToJavaStatement = ex -> {

    // TODO: distinguish whether its a function call via the grammar!
    if (ex.functioncall() != null) {
      // set the function's name (e.g. 'doSomething' for 'bla = doSomething();' ):
      final String functionName = ex.functioncall().ID().getText();
      final FunctionCall functionCall = new FunctionCall(functionName);
      if (ex.functioncall().parameterlist() != null) {
        functionCall.getParameters()
            .add(ex.functioncall().parameterlist().parameter(0).getText()); //TODO!
      }
      return functionCall;
    }

    // its a mere assignment:
    if (ex.lhs() != null) {
      final Assignment stmnt = new Assignment();
      stmnt.setLhs(computeLeftHandSide(ex.lhs()));
      stmnt.setRhs(computeRightHandSide(ex.rhs()));
      return stmnt;
    }

    System.err.println("couldn't determine statement type.....");
    return null;
  };


  /**
   * Maps a left-hand side identifier to a Java identifier.
   */
  private String computeLeftHandSide(c_grammarParser.LhsContext lhs) {
    return lhs.ID().getText();
  }


  /**
   * Maps a right-hand side to a Java RHS.
   */
  private String computeRightHandSide(c_grammarParser.RhsContext rhs) {
    // TODO: composite expressions!
    return rhs.getText();
  }


  @Override
  public JavaLanguageConstruct visitImportheader(c_grammarParser.ImportheaderContext ctx) {
    final String importName = computeImportName(ctx);
    final Import jimport = new Import(importName);
    javaProgram.getImports().add(jimport);
    return jimport;
  }


  /**
   * Maps a C include to a Java's import file name.
   */
  private String computeImportName(c_grammarParser.ImportheaderContext include) {
    final String includeWithoutSeperatingDot = include.FILEID()
        .getText().replaceAll("\\.", "_");
    final String includeWithoutSlashes = includeWithoutSeperatingDot
        .replaceAll("/", "_");
    final String includeWithoutDashes = includeWithoutSlashes
        .replaceAll("-", "_");
    return includeWithoutDashes;
  }

//  @Override
//  public JavaLanguageConstruct visitChildren(RuleNode node) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitTerminal(TerminalNode node) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitErrorNode(ErrorNode node) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitProgram(ProgramContext ctx) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitImports(ImportsContext ctx) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitFunctionlist(FunctionlistContext ctx) {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  @Override
//  public JavaLanguageConstruct visitBlock(BlockContext ctx) {
//    // TODO Auto-generated method stub
//    return null;
//  }

}
