package de.netherspace.apps.actojat;

import de.netherspace.apps.actojat.ir.java.BasicConstruct;
import de.netherspace.apps.actojat.ir.java.JavaConstructType;
import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct;
import de.netherspace.apps.actojat.ir.java.Program;
import de.netherspace.apps.actojat.util.IntermediateRepresentationException;
import de.netherspace.apps.actojat.util.ParserException;
import de.netherspace.apps.actojat.util.SourceErrorHandler;
import de.netherspace.apps.actojat.util.SourceErrorListener;
import de.netherspace.apps.actojat.util.SourceGenerationException;
import kotlin.Pair;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenFactory;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.UnbufferedCharStream;
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor;
import org.antlr.v4.runtime.tree.ParseTree;
import org.slf4j.Logger;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;


/**
 * An abstract class that holds all general parsing and mapping code to generate a new Java
 * class for a given piece of source code in an arbitrary language.
 *
 * @param <L> A lexer implementation
 * @param <P> A parser implementation
 * @param <C> The corresponding rule context
 * @param <V> A visitor implementation
 */
public abstract class AbstractSourceTranspiler<L extends Lexer,
    P extends Parser,
    C extends ParserRuleContext,
    V extends AbstractParseTreeVisitor<JavaLanguageConstruct>> implements SourceTranspiler {

  protected Logger log;

  private Function<CharStream, L> lexerFactoryExpr;
  private Function<CommonTokenStream, P> parserFactoryExpr;
  private Function<P, C> startsymbolExpr;
  private Supplier<V> visitorFactoryExpr;
  private List<String> ruleNames;
  private Supplier<Map<String, Pair<BasicConstruct, JavaConstructType>>> systemFunctionsSupplier;


  /**
   * The constructor.
   *
   * @param lexerFactoryExpr        A function for creating new lexer instances
   * @param parserFactoryExpr       A function for creating new parser instances
   * @param startsymbolExpr         A function for calling the grammar's start symbol
   * @param visitorFactoryExpr      A supplier for creating new visitor instances
   * @param systemFunctionsSupplier A map that maps functions from the source language to Java
   */
  public AbstractSourceTranspiler(Function<CharStream, L> lexerFactoryExpr,
                                  Function<CommonTokenStream, P> parserFactoryExpr,
                                  Function<P, C> startsymbolExpr,
                                  Supplier<V> visitorFactoryExpr,
                                  Supplier<Map<String, Pair<BasicConstruct, JavaConstructType>>> systemFunctionsSupplier) {
    super();
    this.lexerFactoryExpr = lexerFactoryExpr;
    this.parserFactoryExpr = parserFactoryExpr;
    this.startsymbolExpr = startsymbolExpr;
    this.visitorFactoryExpr = visitorFactoryExpr;
    this.systemFunctionsSupplier = systemFunctionsSupplier;
  }


  @Override
  public ParseTree parseInputStream(InputStream inputStream) throws ParserException,
      FileNotFoundException {
    if (inputStream == null) {
      throw new FileNotFoundException("The input stream was null!");
    }

    log.info("Starting to parse input stream...");
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
    log.debug("\n " + parseTree.toStringTree(parser) + "\n");

    //did an error occur during parsing?
    if (errorHandler.isErrorFlag()
        || errorListener.isErrorFlag()
        || lexerErrorListener.isErrorFlag()) {
      log.error("I couldn't parse the given piece of source code!");
      log.debug("  errorHandler.isErrorFlag() = " + errorHandler.isErrorFlag());
      log.debug("  errorListener.isErrorFlag() = " + errorListener.isErrorFlag());
      log.debug("  lexerErrorListener.isErrorFlag() = " + lexerErrorListener.isErrorFlag());
      throw new ParserException();
    }
    log.debug("I could successfully parse the given piece of source code");

    return parseTree;
  }


  @Override
  public JavaLanguageConstruct generateIntermediateJavaRepresentation(ParseTree parseTree)
      throws IntermediateRepresentationException {
    //walk the tree via the provided visitor implementation:
    V visitor = visitorFactoryExpr.get();
    JavaLanguageConstruct program = visitor.visit(parseTree);
    if (program == null) {
      log.error("I couldn't walk the whole parse tree!");
      throw new IntermediateRepresentationException();
    }
    return program;
  }

  @Override
  public String generateSourceCode(JavaLanguageConstruct program, String name, String basePackage)
      throws SourceGenerationException {
    final Map<String, Pair<BasicConstruct, JavaConstructType>> systemFunctions
        = systemFunctionsSupplier.get();
    final JavaIrToSourceCodeTranslator irTranslator
        = new JavaIrToSourceCodeTranslatorImpl(systemFunctions);

    if (!(program instanceof Program)) {
      log.error("The given JavaLanguageConstruct is not of type Program!");
      throw new SourceGenerationException();
    }

    final String code = irTranslator.generateCodeFromIr((Program) program, name, basePackage);
    if (code == null) {
      log.error("I couldn't generate the actual Java code!");
      throw new SourceGenerationException();
    }
    return code;
  }

  @Override
  public List<File> enrichSourceCode(String code) {
    log.debug(code);
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
