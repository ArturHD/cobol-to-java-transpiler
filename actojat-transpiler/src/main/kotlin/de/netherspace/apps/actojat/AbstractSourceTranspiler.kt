package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.BasicConstruct
import de.netherspace.apps.actojat.ir.java.JavaConstructType
import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct
import de.netherspace.apps.actojat.ir.java.Program
import de.netherspace.apps.actojat.util.*
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor
import org.antlr.v4.runtime.tree.ParseTree
import org.slf4j.Logger
import java.io.File
import java.io.InputStream

/**
 * An abstract class that holds all general parsing and mapping code to generate a new Java
 * class for a given piece of source code in an arbitrary language.
 *
 * @param <L> A lexer implementation
 * @param <P> A parser implementation
 * @param <C> The corresponding rule context
 * @param <V> A visitor implementation
 */
abstract class AbstractSourceTranspiler<L, P, C, V>(
        private val lexerFactoryExpr: java.util.function.Function<CharStream, L>,
        private val parserFactoryExpr: java.util.function.Function<CommonTokenStream, P>,
        private val startsymbolExpr: java.util.function.Function<P, C>,
        private val visitorFactoryExpr: java.util.function.Supplier<V>,
        private val systemFunctionsSupplier: java.util.function.Supplier<Map<String, Pair<BasicConstruct, JavaConstructType>>>,
        private val log: Logger
) : SourceTranspiler where L : Lexer, P : Parser, C : ParserRuleContext, V : AbstractParseTreeVisitor<JavaLanguageConstruct> {

    private lateinit var ruleNames: List<String>

    override fun parseInputStream(inputStream: InputStream): Result<ParseTree> {
        log.info("Starting to parse input stream...")
        val inputCharStream = UnbufferedCharStream(inputStream)

        // create lexer and token stream:
        val lexer: L = lexerFactoryExpr.apply(inputCharStream)
        lexer.tokenFactory = CommonTokenFactory(true) // circumvents a bug in ANTLR v4.7!
        val lexerErrorListener = SourceErrorListener()
        lexer.addErrorListener(lexerErrorListener)
        val tokenStream: CommonTokenStream = CommonTokenStream(lexer)

        // create parser:
        val parser = parserFactoryExpr.apply(tokenStream)
        val errorHandler = SourceErrorHandler()
        parser.errorHandler = errorHandler
        val errorListener = SourceErrorListener()
        parser.addErrorListener(errorListener)
        ruleNames = parser.ruleNames.toList()

        // create the parse tree by calling the start symbol:
        val parseTree = startsymbolExpr.apply(parser)
        log.debug("\n ${parseTree.toStringTree(parser)}\n")

        //did an error occur during parsing?
        if (errorHandler.isErrorFlag()
                || errorListener.isErrorFlag()
                || lexerErrorListener.isErrorFlag()) {
            val m = "I couldn't parse the given piece of source code!"
            log.error(m)
            log.debug("  errorHandler.isErrorFlag() = ${errorHandler.isErrorFlag()}")
            log.debug("  errorListener.isErrorFlag() = ${errorListener.isErrorFlag()}")
            log.debug("  lexerErrorListener.isErrorFlag() = ${lexerErrorListener.isErrorFlag()}")
            return Result.failure(ParserException(m))
        }

        log.debug("I could successfully parse the given piece of source code")
        return Result.success(parseTree)
    }

    override fun generateIntermediateJavaRepresentation(parseTree: ParseTree): Result<JavaLanguageConstruct> {
        //walk the tree via the provided visitor implementation:
        val visitor: V = visitorFactoryExpr.get()
        val program = visitor.visit(parseTree)
        return if (program == null) {
            val m = "I couldn't walk the whole parse tree!"
            log.error(m)
            Result.failure(IntermediateRepresentationException(m))
        } else {
            Result.success(program)
        }
    }

    override fun generateSourceCode(program: JavaLanguageConstruct, name: String, basePackage: String): Result<String> {
        if (program !is Program) {
            val m = "The given JavaLanguageConstruct is not of type Program!"
            log.error(m)
            return Result.failure(SourceGenerationException(m))
        }

        val irTranslator: JavaIrToSourceCodeTranslator = JavaIrToSourceCodeTranslatorImpl(systemFunctionsSupplier.get())

        return irTranslator.generateCodeFromIr(program = program,
                basePackage = basePackage,
                className = name)
    }

    override fun enrichSourceCode(code: String): List<File> {
        log.debug(code)
        // TODO: do autoformatting of the generated source: org.eclipse.jdt.core.formatter
        // TODO: create Maven project
        // TODO:...

        TODO("not implemented")
    }

    override fun getRuleNames(): List<String> {
        return ruleNames
    }
}
