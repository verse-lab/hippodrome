package org.racerdfix.utils

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token, TokenStreamRewriter}
import org.racerdfix.FixConfig
import org.racerdfix.antlr.{Java8Lexer, Java8Parser}
import org.racerdfix.antlr.Java8Parser.CompilationUnitContext
import org.racerdfix.language.{FixKind, InsAfter, InsBefore, Replace, RewriteKind}

import scala.collection.mutable
import scala.io.Source

class FileModif(val filename: String, val rewriter: TokenStreamRewriter)
class PatchIndices(var indices: List[Int] = List.empty) {
  def addOne(): Unit ={
    this.indices = this.indices ++ List(this.indices.length)
  }
}
class RewriterExt(var rewriter: TokenStreamRewriter, var instructions: mutable.Stack[PatchIndices] =  new mutable.Stack[PatchIndices]().push(new PatchIndices())) {
  def addInstructionToTheStack() = {
    val indices = this.instructions.pop()
    indices.addOne()
    this.instructions.push(indices)
  }

  def removeLastPatch() = {
    val instructions = this.instructions.pop().indices.reverse
    var i = 0
    for (i <- instructions) {
      this.rewriter.rollback(i)
    }
  }

  def addInstruction(fixKind: RewriteKind, start: Token, stop: Token, patch: String) = {
    fixKind match{
      case Replace   =>
        this.rewriter.replace( start.getTokenIndex, stop.getTokenIndex, patch)
      case InsBefore => this.rewriter.insertBefore( start.getTokenIndex, patch)
      case InsAfter  => this.rewriter.insertAfter( stop.getTokenIndex, patch)
    }
    addInstructionToTheStack()
  }

  def initPatch() = {
    this.instructions.push(new PatchIndices())
  }

}
class ASTStoreElem(val tokens: CommonTokenStream, val tree: CompilationUnitContext, var rewriter: RewriterExt)

class ASTManipulation {
  val map = new mutable.HashMap[String,ASTStoreElem]()

  def initPatch() = {
    this.map.foreach( x => x._2.rewriter.initPatch())
  }

  def retrieveAST(filename: String) = {
    map.getOrElseUpdate(filename,parseContent(filename))
  }

  def rollbackLastPatch() = {
    this.map.foreach(x => x._2.rewriter.removeLastPatch())
    initPatch()
  }

  def parseContent(filename: String) = {
    val javaClassContent = Source.fromFile(filename).getLines.foldLeft("") { (str, line) => str + " \n " + line.toString }
    val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    val rewriter = new TokenStreamRewriter(tokens)
    new ASTStoreElem(tokens, tree, new RewriterExt(rewriter))
  }

  def dumpToFile(filename: String, config: FixConfig, copy_original: Boolean =  false, copy_to_temp: Boolean = false) = {
    val fm = new FileManipulation
    val astElem = map.get(filename)
    astElem match {
      case None =>
      case Some(astElem) =>
          /* write to file (keep the original one in `filename` and the fix in `filename.fix` */
          if (copy_original) fm.cloneOriginalFile(filename)
          if (copy_to_temp)  fm.cloneOriginalToTemp(filename)
          fm.overwriteFile(filename, astElem.rewriter.rewriter.getText)
    }
  }

  def revertFile(filename: String, config: FixConfig) = {
    val fm = new FileManipulation
    val astElem = map.get(filename)
    astElem match {
      case None =>
      case Some(_) =>
        /* write to file (keep the original one in `filename` and the fix in `filename.fix` */
        fm.revertToOriginalFile(filename)
    }
  }

  def dumpAll(config: FixConfig, copy_original: Boolean = false) = {
    if(!config.intellij)  {
      map.foreachEntry[Unit]((filename, _) => dumpToFile(filename, config, copy_original))
    }
  }

  def dumpAllTemp(config: FixConfig) = {
    if(!config.intellij)  {
      map.foreachEntry[Unit]((filename, _) => dumpToFile(filename, config, copy_to_temp = true))
    }
  }

  def saveOriginal(config: FixConfig) = {
    if(!config.intellij)  {
      map.foreachEntry[Unit]((filename, _) => {
        val fm = new FileManipulation
        fm.cloneOriginalFile(filename)
      })
    }
  }

  /* TODO: no need for ast here */
  def resetToOrig (config: FixConfig) = {
    if(!config.intellij)
      map.foreachEntry[Unit]((filename, _) => revertFile(filename, config))
  }

  /* TODO: no need for ast here */
  def resetFromTempToOrig (config: FixConfig) = {
    if(!config.intellij)
      map.foreachEntry[Unit]((filename, _) => {
        val fm = new FileManipulation
        fm.revertFromTempFile(filename)
      })
  }
}
