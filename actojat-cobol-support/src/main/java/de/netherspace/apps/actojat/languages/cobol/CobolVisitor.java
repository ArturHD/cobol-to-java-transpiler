package de.netherspace.apps.actojat.languages.cobol;

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.function.Function;


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


  @Override
  public JavaLanguageConstruct visitSection(cobol_grammarParser.SectionContext ctx) {
    final String methodName = computeMethodName(ctx.sectiondecl());
    final Method javaMethod = new Method(methodName);
    javaProgram.getMethods().add(javaMethod);

    final String body = ctx.block().getText(); //TODO: how to descent????
    final JavaLanguageConstruct statements = super.visitSection(ctx);
    //javaMethod.getStatements().add(new Statement());

    return javaMethod;
  }


  /**
   * Maps a Cobol section's signature to a Java method name.
   */
  private String computeMethodName(cobol_grammarParser.SectiondeclContext sectiondecl) {
    final String methodPrefix = "section_";// TODO: Cobol sections might start with digits...
    final String methodNameWithoutSectionKeyword = sectiondecl.SECTIONNAME().getText();
    final String transformedSectiondecl = methodNameWithoutSectionKeyword
        .replaceAll("-", "_");
    return methodPrefix + transformedSectiondecl;
  }


  @Override
  public JavaLanguageConstruct visitSectionlist(cobol_grammarParser.SectionlistContext ctx) {
    //TODO: handle the "... USING ..." in "PROCEDURE DIVISION USING REQUEST-SATZ." ?!
    return super.visitSectionlist(ctx);
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


//    @Override
//    public JavaLanguageConstruct visitImports(ImportsContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitTailingimports(TailingimportsContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visit(ParseTree tree) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitChildren(RuleNode node) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitTerminal(TerminalNode node) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitErrorNode(ErrorNode node) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitProgram(ProgramContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitSectionlist(SectionlistContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitSection(SectionContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitSectionend(SectionendContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitSectiondecl(SectiondeclContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitBlock(BlockContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }
//
//    @Override
//    public JavaLanguageConstruct visitStatements(StatementsContext ctx) {
//        // TODO Auto-generated method stub
//        return null;
//    }

}
