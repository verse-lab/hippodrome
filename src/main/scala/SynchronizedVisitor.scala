package org.racerdfix

import org.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import org.antlr.v4.runtime.{CommonTokenStream, TokenStreamRewriter}
import org.antlr.v4.runtime.misc.Interval
import org.racerdfix.language.{FixKind, Insert, NoFix, Update}

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None
  private var resource: Option[Java8Parser.ExpressionNameContext] = None
  private var resourceStatement: Option[Java8Parser.StatementContext] = None

  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  def getSynchronizedBlock = sblock
  def getResource = resource
  def getResourceStatement = resourceStatement

  override def visitSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext): Unit = {
    fix match {
      case Update(cls, line, lock_old, lock_new) => {
        val block = ctx.block()
        if (Globals.getRealLineNo(block.getStart.getLine) <= line && line <= Globals.getRealLineNo(block.getStop.getLine)) {
          if (lock_old== ctx.expression().getText)
            sblock = Some (ctx)
        }
      }
      case Insert(_, _, _, _) =>
      case NoFix =>
    }
    super.visitChildren(ctx)
  }

  /* TODO: need to check more granular on the var names (myA.f is not recognized as a child of myA) */
  override def visitExpressionName(ctx: Java8Parser.ExpressionNameContext): Unit = {
    fix match {
      case Insert(cls, line, unprotected_resource, lock_new) => {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)){
          val ctxStr = ctx.getText
          if (unprotected_resource == ctxStr){
            resource = Some(ctx)
          }
        }
      }
      case Update(cls, line, lock_old, lock_new) =>
      case NoFix =>
    }
    super.visitChildren(ctx)
  }

  /* capture the inner most statement which contains the culprit resource */
  override def visitStatement(ctx: Java8Parser.StatementContext): Unit = {
    this.visitChildren(ctx)
    resource match {
      case None      =>
      case Some(res) => {
        resource = None
        resourceStatement = Some(ctx)
      }
    }
  }

  def insertSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.StatementContext, fix: Insert): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

//    println("ctx:" + ctx.start.getInputStream.getText(interval))


    rewriter.insertBefore(ctx.start, "synchronized(" + fix.lock + ") {")
    rewriter.insertAfter(ctx.stop, "}")
    val rinterval = new Interval(ctx.getSourceInterval.a,ctx.getSourceInterval.b+1)

//    println("ctx: " + ctx.start.getInputStream.getText(interval)  + "#####")
//    println("rewriter:" + rewriter.getText())
//    println("rewriter:" + rewriter.getText(rinterval) + "#####")

    (ctx.start.getInputStream.getText(interval),rewriter.getText(rinterval))
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
