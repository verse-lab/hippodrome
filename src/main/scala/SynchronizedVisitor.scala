package com.racerdfix

import com.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import com.racerdfix.fixdsl._
import org.antlr.v4.runtime.TokenStreamRewriter
import org.antlr.v4.runtime.misc.Interval

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None

  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  /**
   *  There si a discrepancy between the line number returned by RacerDFix (the real line number)
   *  and that returned by antlr which if off by 1 (upwards). To relate to the real line number
   *  we must step up by 1 whatever is indicated by RacerDFix.
   */
  def getRealLineNo(lineno: Int) = lineno - 1

  def getSynchronizedBlock = sblock

  override def visitSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext): Unit = {
    val line = fix match {
      case Update(cls, line, lock_old, lock_new) => line
      case Insert(cls, line, lock) => line
      case NoFix => 0
    }
    val block = ctx.block()
    if (getRealLineNo(block.getStart.getLine) <= line && line <= getRealLineNo(block.getStop.getLine)) {
      sblock = Some (ctx)
    }
    println("Line Start:" + ctx.expression().start.getLine + " expression: " + ctx.expression().getText)
    println("Line Start:" + block.getStart.getLine)
    println("Line Stop:"  + block.getStop.getLine)
    println("block: "     + ctx.getText)
    println("Parent: "    + ctx.getParent.getParent.getText)
    super.visitChildren(ctx)
  }

  def updateSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext, fix: Update): Java8Parser.SynchronizedStatementContext = {
    println("Parent: " + ctx.getParent.getParent.getParent.getText)
    println("sblock: " + ctx.getText)
    println("start: "  + ctx.start)
    println("stop: "   + ctx.stop)

    val a: Int = ctx.start.getStartIndex
    val b: Int = ctx.stop.getStopIndex
    val interval: Interval = new Interval(a, b)

    println("ctx:" + ctx.start.getInputStream.getText(interval))

//    val rewriter = new TokenStreamRewriter(ctx.start.getInputStream.getTokens())
//    sblock match {
//      case Some(sblock) => {
//        rewriter.replace(sblock.start, sblock.stop, patch)
//        println("FIX:" + rewriter.getText())
//      }
//      case None =>
//    }

    ctx
  }

  override def visitImportDeclaration(ctx: Java8Parser.ImportDeclarationContext): Unit = {
    println("Import line:"  + ctx.start.getLine)
  }
}
