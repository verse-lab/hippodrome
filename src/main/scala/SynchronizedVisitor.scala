package org.racerdfix

import org.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import org.antlr.v4.runtime.{CommonTokenStream, TokenStreamRewriter}
import org.antlr.v4.runtime.misc.Interval
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.language.{FixKind, InsertDeclareAndInst, InsertSync, NoFix, UpdateSync}

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None
  private var resource: Option[Java8Parser.ExpressionNameContext] = None
  private var resourceStatement: Option[Java8Parser.StatementContext] = None
  private var classStmt: Option[Java8Parser.ClassDeclarationContext] = None

  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  def getSynchronizedBlock = sblock
  def getResource = resource
  def getResourceStatement = resourceStatement
  def getClassStatement = classStmt
  def getClassStart = classStmt match {
    case Some(cls) => Some (cls.normalClassDeclaration().classBody().start )
    case None => None
  }

  override def visitSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext): Unit = {
    fix match {
      case UpdateSync(cls, line, lock_old, lock_new) => {
        val block = ctx.block()
        if (Globals.getRealLineNo(block.getStart.getLine) <= line && line <= Globals.getRealLineNo(block.getStop.getLine)) {
          if (lock_old== ctx.expression().getText)
            sblock = Some (ctx)
        }
      }
      case InsertSync(_, _, _, _) =>
      case _ =>
    }
    this.visitChildren(ctx)
  }

  override def visitExpressionName(ctx: Java8Parser.ExpressionNameContext): Unit = {
//    println("VISIT EXPRESSION " + ctx.getText)
//    println(ctx.children)
    fix match {
      case InsertSync(cls, line, unprotected_resource, lock_new) => {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)){
          val vars = RacerDAPI.refToListOfRef(ctx.getText)
          if (vars.contains(unprotected_resource)){
            resource = Some(ctx)
          }
        }
      }
      case InsertDeclareAndInst(cls,line,_,_) =>  {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)){
          resource = Some(ctx)
        }
      }
      case UpdateSync(cls, line, lock_old, lock_new) =>
      case _ =>
    }
    this.visitChildren(ctx)
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

  override def visitClassDeclaration(ctx: Java8Parser.ClassDeclarationContext): Unit = {
    fix match {
      case InsertDeclareAndInst(cls,line,_,_) =>  {
        val classes = RacerDAPI.classToListOfCls(cls)
        if (classes.contains(ctx.normalClassDeclaration().Identifier().getText)){
          classStmt = Some(ctx)
        }
      }
      case _ =>
    }
    this.visitChildren(ctx)
  }

  override def visitFieldAccess(ctx: Java8Parser.FieldAccessContext): Unit = {
//    println("Field Access" + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitFieldAccess_lfno_primary(ctx: Java8Parser.FieldAccess_lfno_primaryContext): Unit = {
//    println("Field Access_lfno_primary" + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitFieldAccess_lf_primary(ctx: Java8Parser.FieldAccess_lf_primaryContext): Unit = {
//    println("Field Access_lf_primary" + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitResource(ctx: Java8Parser.ResourceContext): Unit = {
    this.visitChildren(ctx)
//    println("Resource" + ctx.getText)
  }

  def insertSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.StatementContext, fix: InsertSync): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

//    println("ctx:" + ctx.start.getInputStream.getText(interval))

    rewriter.insertBefore(ctx.start, "synchronized(" + fix.lock + ") { ")
    rewriter.insertAfter(ctx.stop, " } ")

    val rinterval = new Interval(ctx.getSourceInterval.a,ctx.getSourceInterval.b+1)

//    println("ctx: " + ctx.start.getInputStream.getText(interval)  + "#####")
//    println("rewriter:" + rewriter.getText())
//    println("rewriter:" + rewriter.getText(rinterval) + "#####")

    (ctx.start.getInputStream.getText(interval),rewriter.getText(rinterval))
  }

  def insertInsertDeclareAndInstStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.ClassDeclarationContext, fix: InsertDeclareAndInst): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

    //    println("ctx:" + ctx.start.getInputStream.getText(interval))

    // only work for object now, since it has no arguments
    val textToInsert = fix.typ + " " + fix.variable + " = " + " new " + fix.typ + "(); "
    rewriter.insertAfter(ctx.normalClassDeclaration.classBody().start, textToInsert)

    //val rinterval = new Interval(ctx.getSourceInterval.a,ctx.getSourceInterval.a)

    //    println("ctx: " + ctx.start.getInputStream.getText(interval)  + "#####")
    //    println("rewriter:" + rewriter.getText())
    //    println("rewriter:" + rewriter.getText(rinterval) + "#####")

    (ctx.start.getInputStream.getText(interval),textToInsert)
  }



  def updateSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.SynchronizedStatementContext, fix: UpdateSync): (String,String) = {
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
