package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Argument;
import de.netherspace.apps.actojat.intermediaterepresentation.java.FunctionCall;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Statement;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CobolVisitor extends cobol_grammarBaseVisitor<JavaLanguageConstruct> {

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
    final String methodName = computeMethodName(ctx);
    final Method javaMethod = new Method(methodName);

    final List<Statement> javaStatements = ctx
        .sentence()
        .stream()
        .map(this.sentenceToJavaStatement)
        .collect(Collectors.toList());
    //TODO: keep order!
    javaMethod.getStatements().addAll(javaStatements);

    javaProgram.getMethods().add(javaMethod);
    return javaMethod;
  }


  /**
   * Maps a COBOL sentence to a Java statement.
   */
  private final Function<cobol_grammarParser.SentenceContext, Statement> sentenceToJavaStatement
      = sentence -> {
        final cobol_grammarParser.StatementContext cobolStatement = sentence.statement().get(0);
        final String functionName = cobolStatement.operation().getText();
        final List<Argument> parameters = cobolStatement
            .operand()
            .stream()
            .map(this.operandToJavaArgument)
            .collect(Collectors.toList());
        System.out.println("\n\n functionName: " + functionName);
        /*
         * TODO: A) "DISPLAY", "MV" etc should have their own rule - then we can easy switch-case!
         * TODO: B) how to split multiple COBOL statements? By "DISPLAY", ... tokens?
         */
        FunctionCall functionCall = new FunctionCall(functionName);
        // TODO: add parameters...
        return functionCall;
      };


  /**
   * Maps a COBOL operand (e.g. the "HelloWorld" in
   * DISPLAY "HelloWorld"
   * ) to Java argument.
   */
  private final Function<cobol_grammarParser.OperandContext, Argument> operandToJavaArgument
      = op -> {
        final String name = op.getText();
        final String type = ""; // TODO: map the actual type!
        return new Argument(type, name);
      };


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
