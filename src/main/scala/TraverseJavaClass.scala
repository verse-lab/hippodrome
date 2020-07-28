package com.racerdfix

import com.racerdfix.antlr.{Java8Lexer, Java8Parser}
import com.racerdfix.fixdsl.Update
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, TokenStreamRewriter}

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

  def parseContent(content: String) ={
    val java8Lexer = new Java8Lexer(CharStreams.fromString(content))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    (tokens,tree)
  }

  def traverseFileVisitor(filename: String){
    /* ******** */
    /*   PARSE  */
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val (tokens,tree) = parseContent(javaClassContent)

    /* ************** */
    /* GENERATE PATCH */
    val update = Update("RacyFalseNeg",26,"myA2", "myA3")
    val syncVisitor = new SynchronizedVisitor
    
    syncVisitor.setFix(update)
    syncVisitor.visit(tree)
    val sblock      = syncVisitor.getSynchronizedBlock
    sblock match {
      case Some(sblock) =>     val new_sbblock = syncVisitor.updateSynchronizedStatement(sblock,update)
        println("new sblock: " + new_sbblock.getText)
      case None => println("None")
    }

    println("==========================================")
    val txt = sblock match {
      case Some(sblock) => sblock.getText
      case None => "None"
    }
    println("sblock:" + txt)

    /* manually crafted patch */
    val patch = "synchronized(myA3){myA=a;myA=a;myA=a}"

    /* ************ */
    /* GENERATE FIX */
    val rewriter = new TokenStreamRewriter(tokens)
    sblock match {
      case Some(sblock) => {
        rewriter.replace(sblock.start, sblock.stop, patch)
        println("FIX:" + rewriter.getText())
      }
      case None =>
    }

  }

  def main(args: Array[String]) = {
    println("Testing")
    //traverseFileListener("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    traverseFileVisitor("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
  }
}
