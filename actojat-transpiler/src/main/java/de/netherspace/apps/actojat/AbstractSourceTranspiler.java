package de.netherspace.apps.actojat;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import de.netherspace.apps.actojat.intermediaterepresentation.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.util.*;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenFactory;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.UnbufferedCharStream;
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.logging.log4j.Logger;


/**
 * An abstract class that holds all general parsing and mapping code to generate a new Java
 * class for a given piece of source code in an arbitrary language.
 * 
 * @param <L> A lexer implementation
 * @param <P> A parser implementation
 * @param <C> The corresponding rule context
 * @param <V> A visitor implementation
 */
public abstract class AbstractSourceTranspiler<L extends Lexer, P extends Parser, C extends ParserRuleContext, V extends AbstractParseTreeVisitor<JavaLanguageConstruct>> implements SourceTranspiler {
	
	protected Logger logger;
	
	private Function<CharStream, L> lexerFactoryExpr;
	private Function<CommonTokenStream, P> parserFactoryExpr;
	private Function<P, C> startsymbolExpr;
	private Supplier<V> visitorFactoryExpr;
	private List<String> ruleNames;
	

	/**
	 * The constructor.
	 * 
	 * @param lexerFactoryExpr A lambda expression for creating new lexer instances
	 * @param parserFactoryExpr A lambda expression for creating new parser instances
	 * @param startsymbolExpr A lambda expression for calling the grammar's start symbol
	 * @param visitorFactoryExpr A lambda expression for creating new visitor instances
	 */
	public AbstractSourceTranspiler(Function<CharStream, L> lexerFactoryExpr,
				Function<CommonTokenStream, P> parserFactoryExpr,
				Function<P, C> startsymbolExpr,
				Supplier<V> visitorFactoryExpr) {
		super();
		this.lexerFactoryExpr = lexerFactoryExpr;
		this.parserFactoryExpr = parserFactoryExpr;
		this.startsymbolExpr = startsymbolExpr;
		this.visitorFactoryExpr = visitorFactoryExpr;
	}


	@Override
	public ParseTree parseInputStream(InputStream inputStream) throws IOException, ParserException {
		logger.info("Starting to parse input stream...");
		CharStream inputCharStream = new UnbufferedCharStream(inputStream);

		// create lexer and token stream:
		L lexer = this.lexerFactoryExpr.apply(inputCharStream);
		lexer.setTokenFactory(new CommonTokenFactory(true)); // circumvents a bug in ANTLR v4.7!
		SourceErrorListener lexerErrorListener = new SourceErrorListener();
		lexer.addErrorListener(lexerErrorListener);
		CommonTokenStream tokenStream = new CommonTokenStream(lexer);

		// create parser:
		P parser = this.parserFactoryExpr.apply(tokenStream);
		SourceErrorHandler errorHandler = new SourceErrorHandler();
		parser.setErrorHandler(errorHandler);
		SourceErrorListener errorListener = new SourceErrorListener();
		parser.addErrorListener(errorListener);
		this.ruleNames = Arrays.asList(parser.getRuleNames());

		// create the parse tree by calling the start symbol:
		ParseTree parseTree = startsymbolExpr.apply(parser);
		logger.debug("\n"+parseTree.toStringTree(parser)+"\n");
		
		//did an error occur during parsing?
		if (parseTree == null || errorHandler.isErrorFlag()
				|| errorListener.isErrorFlag()|| lexerErrorListener.isErrorFlag()) {
			logger.error("I couldn't parse the given piece of source code!");
			throw new ParserException();
		}
		logger.debug("I could successfully parse the given piece of source code");
		
		return parseTree;
	}
	

	@Override
	public JavaLanguageConstruct generateIntermediateJavaRepresentation(ParseTree parseTree) throws IntermediateRepresentationException {
		//walk the tree via the provided visitor implementation:
		V visitor = visitorFactoryExpr.get();
		JavaLanguageConstruct program = visitor.visit(parseTree);
		if (program == null) {
			logger.error("I couldn't walk the whole parse tree!");
			throw new IntermediateRepresentationException();
		}
		return program;
	}

	@Override
	public String generateSourceCode(JavaLanguageConstruct program, String name, String basePackage) throws SourceGenerationException {
		JavaIrToSourceCodeTranslator irTranslator = new JavaIrToSourceCodeTranslator();
		irTranslator.setClassName(name);
		irTranslator.setBasePackage(basePackage);
		
		if (!(program instanceof Program)) {
			logger.error("The given JavaLanguageConstruct is not of type Program!");
			throw new SourceGenerationException();
		}
		
		String code = irTranslator.generateCodeFromIR((Program) program);
		if (code == null) {
			logger.error("I couldn't generate the actual Java code!");
			throw new SourceGenerationException();
		}
		return code;
	}

	@Override
	public List<File> enrichSourceCode(String code) {
		logger.debug(code);
		//TODO: do autoformatting of the generated source: org.eclipse.jdt.core.formatter
		//TODO: create Maven project
		//TODO:...
		return null;
	}


	@Override
	public List<String> getRuleNames() {
		return ruleNames;
	}

}
