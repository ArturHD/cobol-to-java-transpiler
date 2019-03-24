package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Assignment;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Expression;
import de.netherspace.apps.actojat.intermediaterepresentation.java.ForLoop;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Statement;
import de.netherspace.apps.actojat.languages.BaseVisitor;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CobolVisitor extends cobol_grammarBaseVisitor<JavaLanguageConstruct>
                          implements BaseVisitor {

  private Program javaProgram;

  /**
   * The default constructor.
   */
  CobolVisitor() {
    super();
    this.javaProgram = new Program();
  }


  @Override
  public JavaLanguageConstruct visit(ParseTree tree) {
    super.visit(tree);
    return javaProgram;
  }


  /*@Override
  public JavaLanguageConstruct visitSection(cobol_grammarParser.SectionContext ctx) {
    final String methodName = computeMethodName(ctx.sectiondecl());
    final Method javaMethod = new Method(methodName);
    javaProgram.getMethods().add(javaMethod);

    //final String body = ctx.block().getText(); // TODO: how to descent????
    final JavaLanguageConstruct statements = super.visitSection(ctx);
    //javaMethod.getStatements().add(new Statement());

    return javaMethod;
  }*/


  @Override
  public JavaLanguageConstruct visitParagraph(cobol_grammarParser.ParagraphContext ctx) {
    final String sourceName = ctx.ID().getText();
    final String methodName = computeMethodName(ctx);
    final Method javaMethod = new Method(methodName);

    final List<Statement> javaStatements = ctx
        .sentence()
        .stream()
        .map(this.sentenceToJavaStatement)
        .collect(Collectors.toList());
    javaMethod.getStatements().addAll(javaStatements);

    javaProgram.getMethods().put(sourceName, javaMethod);
    return javaMethod;
  }


  /**
   * Maps a COBOL sentence to a Java statement.
   */
  private final Function<cobol_grammarParser.SentenceContext, Statement> sentenceToJavaStatement
      = sentence -> {
        final cobol_grammarParser.StatementContext cobolStatement = sentence.statement().get(0);

        final boolean isDisplayvalueStatement = cobolStatement.displayvalue() != null;
        final boolean isPerformtimesStatement = cobolStatement.performtimes() != null;
        final boolean isPerformuntilStatement = cobolStatement.performuntil() != null;
        final boolean isPerformvaryingStatement = cobolStatement.performvarying() != null;
        final boolean isPerformFunctionStatement = cobolStatement.performsinglefunction() != null;
        final boolean isStopoperationStatement = cobolStatement.stopoperation() != null;

        // "DISPLAY ... ":
        if (isDisplayvalueStatement) {
          final String functionName = "DISPLAY"; // TODO: make an enum holding these values!
          final FunctionCall functionCall = new FunctionCall(functionName);

          final List<Expression> parameters = cobolStatement.displayvalue()
              .STRINGVALUE()
              .stream()
              .map(this.stringvalueToJavaExpression)
              .collect(Collectors.toList());

          functionCall.getParameters().addAll(parameters);
          return functionCall;

          // "PERFORM ... TIMES":
        } else if (isPerformtimesStatement) {
          final cobol_grammarParser.PerformtimesContext performtimes = cobolStatement
              .performtimes();
          final cobol_grammarParser.CounterContext cobolLoopCounter = performtimes.counter();
          final cobol_grammarParser.BlocknameContext blockname = performtimes.blockname();

          final String functionName = blockname.getText();
          final String loopCounter = computeForLoopCounter(cobolLoopCounter);
          final FunctionCall functionCall = new FunctionCall(functionName);

          Statement[] body = { functionCall };
          final Assignment loopVariable = null;
          final String loopCondition = null;
          final String loopIncrement = null;
          return new ForLoop(body, loopVariable, loopCondition, loopIncrement);

          // "PERFORM ... UNTIL":
        } else if (isPerformuntilStatement) {
          // TODO!

          // "STOP ...":
        } else if (isStopoperationStatement) {
          final String functionName = "STOP"; // TODO: make an enum holding these values!
          return new FunctionCall(functionName);
        }

        //        if (cobolStatement.operation().DISPLAY() != null) {
        //          // TODO: ...
        //
        //        } else if (cobolStatement.operation().PERFORM() != null) {
        //          // check the last operand ("PERFORM ... TIMES/UNTIL"):
        //          final cobol_grammarParser.OperandContext lastOperand = cobolStatement
        //              .operand()
        //              .get(cobolStatement.operand().size() - 1);
        //          // Loop or mere function call?
        //          final boolean lastOperandIsTimes = lastOperand.TIMES() != null;
        //          if (lastOperandIsTimes) {
        //            // TODO: FOR loop!
        //            System.out.println("For Loop!!"); // TODO: erase!
        //          }
        //        }
        //
        //        final String functionName = cobolStatement.operation().getText();
        //
        //        // TODO: FunctionCall OR Keyword OR Construct,
        //        // TODO: e.g. Perform -> dispatch , Perform...Times -> for()
        //        FunctionCall functionCall = new FunctionCall(functionName);
        //
        //        final List<Expression> parameters = cobolStatement
        //            .operand()
        //            .stream()
        //            .map(this.operandToJavaExpression)
        //            .collect(Collectors.toList());
        //
        //        functionCall.getParameters().addAll(parameters);
        //        return functionCall;
        return new FunctionCall(cobolStatement.getText());
      };


  /**
   * Maps a COBOL operand (e.g. the "HelloWorld" in
   * DISPLAY "HelloWorld"
   * ) to a Java expression.
   */
  private final Function<cobol_grammarParser.OperandContext, Expression> operandToJavaExpression
      = op -> {
        final String[] parts = { op.getText() };
        return new Expression(parts);
      };


  private Function<? super TerminalNode, Expression> stringvalueToJavaExpression = s -> {
    final String[] parts = { s.getText() };
    return new Expression(parts);
  };


  /**
   * Computes a Java for-loop counter signature.
   *
   * @param cobolLoopCounter the original
   * @return the Java counter signature
   */
  private String computeForLoopCounter(cobol_grammarParser.CounterContext cobolLoopCounter) {
    final String cobolLoopCounterText = cobolLoopCounter.getText();
    final String counter = cobolLoopCounterText; // TODO ...
    return null;
  }


  /**
   * Computes a Java method name from a COBOL section's signature.
   */
  private String computeMethodName(cobol_grammarParser.SectiondeclContext sectiondecl) {
    final String methodPrefix = "section_";// TODO: Cobol sections might start with digits...
    final String methodNameWithoutSectionKeyword = sectiondecl.SECTIONNAME().getText();
    final String transformedSectiondecl = methodNameWithoutSectionKeyword
        .replaceAll("-", "_");
    return methodPrefix + transformedSectiondecl;
  }


  /**
   * Computes a Java method name from a COBOL paragraph.
   */
  private String computeMethodName(cobol_grammarParser.ParagraphContext paragraph) {
    final String methodPrefix = "paragraph_";// TODO: Cobol sections might start with digits...
    final String transformedSectiondecl = paragraph.ID().getText()
        .replaceAll("-", "_");
    return methodPrefix + transformedSectiondecl;
  }


  @Override
  public JavaLanguageConstruct visitProceduredivision(
      cobol_grammarParser.ProceduredivisionContext ctx) {
    return visitChildren(ctx);
  }


  @Override
  public JavaLanguageConstruct visitImportcopyfile(cobol_grammarParser.ImportcopyfileContext ctx) {
    final String importName = computeImportName(ctx);
    final Import jimport = new Import(importName);
    javaProgram.getImports().add(jimport);
    return jimport;
  }

  /**
   * Maps a Cobol import to a Java's import file name.
   */
  private String computeImportName(cobol_grammarParser.ImportcopyfileContext importcopy) {
    final String importPrefix = "cobol_"; // TODO: Cobol imports might start with digits...
    final String importWithoutSeperatingDot = importcopy.FILEID()
        .getText().replaceAll("\\.", "_");
    final String transformedImport = importWithoutSeperatingDot
        .replaceAll("-", "_");
    return importPrefix + transformedImport;
  }

}
