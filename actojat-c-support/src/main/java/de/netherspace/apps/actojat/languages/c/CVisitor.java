package de.netherspace.apps.actojat.languages.c;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;

import de.netherspace.apps.actojat.c_grammarBaseVisitor;
import de.netherspace.apps.actojat.c_grammarParser;
import de.netherspace.apps.actojat.intermediaterepresentation.java.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;


/**
 * A visitor implementation that generates an intermediate representation for a
 * particular parse tree.
 */
public class CVisitor extends c_grammarBaseVisitor<JavaLanguageConstruct> {

    private Program javaProgram;


    /**
     * The default constructor.
     */
    public CVisitor() {
        super();
        this.javaProgram = new Program();
    }


    @Override
    public JavaLanguageConstruct visit(ParseTree tree) {
        super.visit(tree);
        return javaProgram;
    }


    /**
     * Maps a C function to a Java method's name.
     */
    Function<c_grammarParser.FunctiondeclrContext, String> functionToJavaMethod = cfunction -> {
        return cfunction.ID().getText();
    };


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
     * Maps a C left-hand side identifier to a Java identifier.
     */
    Function<c_grammarParser.LhsContext, String> lhsToJavaIdentifier = x -> {
        return x.ID().getText();
    };


    /**
     * Maps a C right-hand side to a Java RHS.
     */
    Function<c_grammarParser.RhsContext, String> rhsToJavaIdentifier = y -> {
        return y.getText();
        //TODO: composite expressions!
    };


    /**
     * Maps a C assignment operator to a Java assignm. op.
     */
    Function<TerminalNode, String> assignmentOperatorToJavaOperator = op -> {
        return op.getText();
    };


    /**
     * Maps a C expression to a Java statement.
     */
    Function<c_grammarParser.ExpressionContext, Statement> expressionToJavaStatement = ex -> {
        if (ex.functioncall() != null) {
            FunctionCall functionCall = new FunctionCall();
            functionCall.setName(ex.functioncall().ID().getText()); //TODO!
            if (ex.functioncall().parameterlist() != null) {
                functionCall.getParameters().add(ex.functioncall().parameterlist().parameter(0).getText()); //TODO!
            }
            return functionCall;
        }
        if (ex.lhs() != null) {
            Assignment stmnt = new Assignment();
            stmnt.setLhs(lhsToJavaIdentifier.apply(ex.lhs()));
            stmnt.setRhs(rhsToJavaIdentifier.apply(ex.rhs()));
            return stmnt;
        }

        System.err.println("coudln't determine statement type.....");
        return null;
    };


    @Override
    public JavaLanguageConstruct visitFunctiondeclr(c_grammarParser.FunctiondeclrContext ctx) {
        Method javaMethod = new Method();
        javaMethod.setName(functionToJavaMethod.apply(ctx));

//		List<Argument> jarguments = argumentsToJavaArgs.apply(ctx.argumentlist())
//													   .entrySet()
//													   .stream()
//													   .map(argEntryToJavaArgument)
//													   .collect(Collectors.toList());
//		//TODO: keep order!
//		javaMethod.getArguments().addAll(jarguments);

        List<Statement> jstatements = ctx.block().expressionlist()
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
     * Maps a C include to a Java's import file name.
     */
    Function<c_grammarParser.ImportheaderContext, String> includeToJavaImportName = include -> {
        String includeWithoutSeperatingDot = include.FILEID().getText().replaceAll("\\.", "_");
        String includeWithoutSlashes = includeWithoutSeperatingDot.replaceAll("/", "_");
        String includeWithoutDashes = includeWithoutSlashes.replaceAll("-", "_");
        return includeWithoutDashes;
    };


    @Override
    public JavaLanguageConstruct visitImportheader(c_grammarParser.ImportheaderContext ctx) {
        Import jimport = new Import();
        jimport.setName(includeToJavaImportName.apply(ctx));
        javaProgram.getImports().add(jimport);
        return jimport;
    }

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
//	public JavaLanguageConstruct visitImports(ImportsContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitFunctionlist(FunctionlistContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	@Override
//	public JavaLanguageConstruct visitBlock(BlockContext ctx) {
//		// TODO Auto-generated method stub
//		return null;
//	}

}
