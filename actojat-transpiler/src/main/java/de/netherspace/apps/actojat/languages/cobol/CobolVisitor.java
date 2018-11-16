package de.netherspace.apps.actojat.languages.cobol;

import java.util.function.Function;

import de.netherspace.apps.actojat.cobol_grammarBaseVisitor;
import de.netherspace.apps.actojat.cobol_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Import;
import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Method;
import de.netherspace.apps.actojat.intermediaterepresentation.java.Program;
import org.antlr.v4.runtime.tree.ParseTree;


/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CobolVisitor extends cobol_grammarBaseVisitor<JavaLanguageConstruct> {

    private Program javaProgram;

    /**
     * The default constructor.
     */
    public CobolVisitor() {
        super();
        this.javaProgram = new Program();
    }


    @Override
    public JavaLanguageConstruct visit(ParseTree tree) {
        super.visit(tree);
        return javaProgram;
    }


    /**
     * Maps a Cobol section's signature to a Java method name.
     */
    Function<cobol_grammarParser.SectiondeclContext, String> sectionToMethodName = sectiondecl -> {
        String methodPrefix = "section_";//Cobol sections might start with digits...
        String methodNameWithoutSectionKeyword = sectiondecl.SECTIONNAME().getText();
        String transformedSectiondecl = methodNameWithoutSectionKeyword.replaceAll("-", "_");
        return methodPrefix + transformedSectiondecl;
    };


    @Override
    public JavaLanguageConstruct visitSection(cobol_grammarParser.SectionContext ctx) {
        cobol_grammarParser.SectiondeclContext sectiondecl = ctx.sectiondecl();
        Method javaMethod = new Method();
        javaMethod.setName(sectionToMethodName.apply(sectiondecl));
        javaProgram.getMethods().add(javaMethod);

        String body = ctx.block().getText(); //TODO: how to descent????
        JavaLanguageConstruct statements = super.visitSection(ctx);
//		javaMethod.getStatements().add(new Statement());

        return javaMethod;
    }


    @Override
    public JavaLanguageConstruct visitSectionlist(cobol_grammarParser.SectionlistContext ctx) {
        //TODO: handle the "... USING ..." in "PROCEDURE DIVISION USING REQUEST-SATZ." ?!
        return super.visitSectionlist(ctx);
    }


    /**
     * Maps a Cobol import to a Java's import file name.
     */
    Function<cobol_grammarParser.ImportcopyfileContext, String> importCopyFileToJavaImportName = importcopy -> {
        String importPrefix = "cobol_"; //Cobol imports might start with digits...
        String importWithoutSeperatingDot = importcopy.FILEID().getText().replaceAll("\\.", "_");
        String transformedImport = importWithoutSeperatingDot.replaceAll("-", "_");
        return importPrefix + transformedImport;
    };


    @Override
    public JavaLanguageConstruct visitImportcopyfile(cobol_grammarParser.ImportcopyfileContext ctx) {
        Import jimport = new Import();
        jimport.setName(importCopyFileToJavaImportName.apply(ctx));
        javaProgram.getImports().add(jimport);
        return jimport;
    }


//	@Override
//	public JavaLanguageConstruct visitImports(ImportsContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitTailingimports(TailingimportsContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visit(ParseTree tree) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitChildren(RuleNode node) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitTerminal(TerminalNode node) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitErrorNode(ErrorNode node) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitProgram(ProgramContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitSectionlist(SectionlistContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitSection(SectionContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitSectionend(SectionendContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitSectiondecl(SectiondeclContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitBlock(BlockContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitStatements(StatementsContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}

}
