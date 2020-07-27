import com.racerdfix.antlr.{Java8Lexer, Java8Parser}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.io.Source

object TraverseJavaClass  {

  def traverseFileListener(filename: String){
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    val walker = new ParseTreeWalker
    val uppercaseMethodListener = new UppercaseMethodListener
    walker.walk(uppercaseMethodListener, tree)

    println("==========================================")
    println("Depth:" + tree.depth())
    println("Error count:" + uppercaseMethodListener.getErrors.size)

    println("==========================================")
    println("==========================================")

  }

  def traverseFileVisitor(filename: String){
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    val uppercaseMethodVisitor = new UppercaseMethodVisitor

    uppercaseMethodVisitor.visit(tree)

    println("==========================================")
    println("Depth:" + tree.depth())
    println("Error count:" + uppercaseMethodVisitor.getErrors.size)
  }

  def main(args: Array[String]) = {
    println("Testing")
    traverseFileListener("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    traverseFileVisitor("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
  }
}
