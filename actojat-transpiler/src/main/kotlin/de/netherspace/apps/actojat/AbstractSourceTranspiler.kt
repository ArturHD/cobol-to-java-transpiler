package de.netherspace.apps.actojat

import de.netherspace.apps.actojat.ir.java.BasicConstruct
import de.netherspace.apps.actojat.ir.java.JavaConstructType
import de.netherspace.apps.actojat.ir.java.JavaLanguageConstruct
import de.netherspace.apps.actojat.ir.java.Clazz
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
        private val lexerFactoryExpr: (CharStream) -> L,
        private val parserFactoryExpr: (CommonTokenStream) -> P,
        private val startsymbolExpr: (P) -> C,
        private val visitorFactoryExpr: () -> V,
        private val systemFunctionsSupplier: () -> Map<String, Pair<BasicConstruct, JavaConstructType>>,
        private val log: Logger
) : SourceTranspiler where L : Lexer, P : Parser, C : ParserRuleContext, V : AbstractParseTreeVisitor<List<JavaLanguageConstruct>> {

    private lateinit var ruleNames: List<String>

    override fun parseInputStream(inputStream: InputStream): Result<ParseTree> {
        log.info("Starting to parse input stream...")
        val inputCharStream = UnbufferedCharStream(inputStream)

        // create lexer and token stream:
        val lexer: L = lexerFactoryExpr(inputCharStream)
        lexer.tokenFactory = CommonTokenFactory(true) // circumvents a bug in ANTLR v4.7!
        val lexerErrorListener = SourceErrorListener()
        lexer.addErrorListener(lexerErrorListener)
        val tokenStream: CommonTokenStream = CommonTokenStream(lexer)

        // create parser:
        val parser = parserFactoryExpr(tokenStream)
        val errorHandler = SourceErrorHandler()
        parser.errorHandler = errorHandler
        val errorListener = SourceErrorListener()
        parser.addErrorListener(errorListener)
        ruleNames = parser.ruleNames.toList()

        // create the parse tree by calling the start symbol:
        val parseTree = startsymbolExpr(parser)
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

    override fun generateIntermediateJavaRepresentation(parseTree: ParseTree): Result<List<JavaLanguageConstruct>> {
        //walk the tree via the provided visitor implementation:
        val visitor: V = visitorFactoryExpr()
        val clazzes = visitor.visit(parseTree)
        return if (clazzes == null) {
            val m = "I couldn't walk the whole parse tree!"
            log.error(m)
            Result.failure(IntermediateRepresentationException(m))
        } else {
            Result.success(clazzes)
        }
    }

    override fun generateSourceCode(clazz: JavaLanguageConstruct, name: String, basePackage: String): Result<String> {
        if (clazz !is Clazz) {
            val m = "The given JavaLanguageConstruct is not of type Clazz!"
            log.error(m)
            return Result.failure(SourceGenerationException(m))
        }

        val irTranslator: JavaIrToSourceCodeTranslator = JavaIrToSourceCodeTranslatorImpl(systemFunctionsSupplier())

        return irTranslator.generateCodeFromIr(
                clazz = clazz,
                basePackage = basePackage,
                className = name
        )
    }

    override fun enrichSourceCode(code: String): String {
        log.debug(code)
        // TODO:...
        return CodeFormatter().formatCode(code)
        // TODO: add a new function that creates a Maven project!
    }

    override fun writeSingleSourceToFile(code: String, dir: String, filename: String): File {
        val f = File(dir, filename)
        log.debug("Writing source to file $f ...")
        if (!f.parentFile.exists()) {
            f.parentFile.mkdirs()
        }
        f.createNewFile()
        f.writeText(code)
        return f
    }

    override fun getRuleNames(): List<String> {
        return ruleNames
    }

}
