package org.racerdfix.utils

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, TokenStreamRewriter}
import org.racerdfix.FixConfig
import org.racerdfix.antlr.{Java8Lexer, Java8Parser}
import org.racerdfix.antlr.Java8Parser.CompilationUnitContext

import scala.collection.mutable
import scala.io.Source

class FileModif(val filename: String, val rewriter: TokenStreamRewriter)
class ASTStoreElem(val tokens: CommonTokenStream, val tree: CompilationUnitContext, var rewriter: TokenStreamRewriter)

class ASTManipulation {
  val map = new mutable.HashMap[String,ASTStoreElem]()

  def retrieveAST(filename: String) = {
    map.getOrElseUpdate(filename,parseContent(filename))
  }

  def parseContent(filename: String) = {
    val javaClassContent = Source.fromFile(filename).getLines.foldLeft("") { (str, line) => str + " \n " + line.toString }
    val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    val rewriter = new TokenStreamRewriter(tokens)
    new ASTStoreElem(tokens, tree, rewriter)
  }

  def dumpToFile(filename: String, config: FixConfig) = {
    val fm = new FileManipulation
    val astElem = map.get(filename)
    astElem match {
      case None =>
      case Some(astElem) =>
        if (!config.testing) {
          /* write to file (keep the original one in `filename` and the fix in `filename.fix` */
          fm.cloneOriginalFile(filename)
          fm.overwriteFile(filename, astElem.rewriter.getText)
        } else {
          /* write to file (keep the original one in `filename.orig` and the fix in `filename` */
          val fixFile = fm.cloneOriginalFileToFix(filename)
          fm.overwriteFile(fixFile, astElem.rewriter.getText)
        }
    }
  }

  def dumpAll(config: FixConfig) = {
    map.foreachEntry[Unit]((filename, _) => dumpToFile(filename, config))
  }

}
