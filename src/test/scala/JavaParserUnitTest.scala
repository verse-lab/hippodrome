import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.junit.Test
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import com.racerdfix.antlr._


class JavaParserUnitTest {
    @Test
    @throws[Exception]
    def whenOneMethodStartsWithUpperCase_thenOneErrorReturned3(): Unit = {
        val javaClassContent = "public class SampleClass { void DoSomething(){} void doSomethingElse(){} }"
        val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
        val tokens = new CommonTokenStream(java8Lexer)
        val java8Parser = new Java8Parser(tokens)
        val tree = java8Parser.compilationUnit
        val walker = new ParseTreeWalker
        val uppercaseMethodListener = new UppercaseMethodListener
        walker.walk(uppercaseMethodListener, tree)
        assertThat(uppercaseMethodListener.getErrors.size, is(1))
        assertThat(uppercaseMethodListener.getErrors.get(0), is("Method DoSomething is uppercased!"))
    }
}
