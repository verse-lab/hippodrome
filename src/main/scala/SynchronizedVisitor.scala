package com.racerdfix

import com.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import com.racerdfix.fixdsl._
import org.antlr.v4.runtime.{CommonTokenStream, TokenStreamRewriter}
import org.antlr.v4.runtime.misc.Interval

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None
  private var assignment: Option[Java8Parser.AssignmentContext] = None

  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  /**
   *  There is a discrepancy between the line number returned by RacerDFix (the real line number)
   *  and that returned by antlr which if off by 1 (upwards). To relate to the real line number
   *  we must step up by 1 whatever is indicated by RacerDFix.
   */
  def getRealLineNo(lineno: Int) = lineno - 1
  def getAntlrLineNo(lineno: Int)= lineno + 1

  def getSynchronizedBlock = sblock
  def getAssignmentBlock = assignment

  override def visitSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext): Unit = {
    fix match {
      case Update(cls, line, lock_old, lock_new) => {
        val block = ctx.block()
        if (getRealLineNo(block.getStart.getLine) <= line && line <= getRealLineNo(block.getStop.getLine)) {
          if (lock_old== ctx.expression().getText)
            sblock = Some (ctx)
        }
//        println("Line Start:" + ctx.expression().start.getLine + " expression: " + ctx.expression().getText)
//        println("Line Start:" + block.getStart.getLine)
//        println("Line Stop:"  + block.getStop.getLine)
//        println("block: "     + ctx.getText)
//        println("Parent: "    + ctx.getParent.getParent.getText)
      }
      case Insert(cls, line, lock) =>
      case NoFix =>
    }
    super.visitChildren(ctx)
  }

  override def visitAssignment(ctx: Java8Parser.AssignmentContext): Unit = {
    fix match {
      case Insert(cls, line, lock_new) => {
        if (getRealLineNo(ctx.start.getLine) <= line && line <= getRealLineNo(ctx.stop.getLine)) {
          assignment = Some(ctx)
        }
        }
      case Update(cls, line, lock_old, lock_new) =>
      case NoFix =>
      }
    println("Assignment: " + ctx.getText)
    super.visitChildren(ctx)
  }

  def insertSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.AssignmentContext, fix: Insert): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

//    println("ctx:" + ctx.start.getInputStream.getText(interval))


    rewriter.insertBefore(ctx.start, "synchronized(" + fix.lock + ") {")
    rewriter.insertAfter(ctx.stop, "}")

    println("ctx: " + ctx.start.getInputStream.getText(interval))
    println("rewriter:" + rewriter.getText(ctx.getSourceInterval))
    //println("rewriter:" + rewriter.getText())

    (ctx.start.getInputStream.getText(interval),rewriter.getText(ctx.getSourceInterval))
  }


  def updateSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.SynchronizedStatementContext, fix: Update): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

//    println("ctx:" + ctx.start.getInputStream.getText(interval))

    val start = ctx.expression().start
    val stop  = ctx.expression().stop
    rewriter.replace(start, stop, fix.lock_new)
    (ctx.start.getInputStream.getText(interval),rewriter.getText(ctx.getSourceInterval))
  }

}
