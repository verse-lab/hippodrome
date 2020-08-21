package org.racerdfix

import com.sun.jndi.toolkit.dir.HierMemDirCtx
import org.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext, TokenStreamRewriter}
import org.antlr.v4.runtime.misc.Interval
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.language.{FixKind, InsertDeclareAndInst, InsertSync, NoFix, Test, UpdateSync}

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None
  private var resource: Option[Any] = None
  private var resourceStatement: Option[Java8Parser.StatementContext] = None
  private var classStmt: Option[Java8Parser.ClassDeclarationContext] = None
  private var static_mthd: Boolean = false
  private var static_ctx: Boolean = false


  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  def getSynchronizedBlock = sblock
  def getResourceStatement = resourceStatement
  def getClassStatement = classStmt
  def getClassStart = classStmt match {
    case Some(cls) => Some (cls.normalClassDeclaration().classBody().start )
    case None => None
  }
  def isStaticCtx = static_ctx
  def getModifiers = if (isStaticCtx) List("static") else Nil

  override def visitSynchronizedStatement(ctx: Java8Parser.SynchronizedStatementContext): Unit = {
    fix match {
      case UpdateSync(_,cls, line, lock_old, lock_new) => {
        val block = ctx.block()
        if (Globals.getRealLineNo(block.getStart.getLine) <= line && line <= Globals.getRealLineNo(block.getStop.getLine)) {
          if (lock_old== ctx.expression().getText)
            sblock = Some (ctx)
        }
      }
      case InsertSync(_,_, _, _, _) =>
      case _ =>
    }
    this.visitChildren(ctx)
  }

  def visitCriticalSection(ctx: ParserRuleContext) = {
    fix match {
      case InsertSync(_,cls, line, unprotected_resource, lock_new) => {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)){
          val vars = RacerDAPI.refToListOfRef(ctx.getText)
          val vars_extended = vars.map(v => if (cls.length >0 ) cls + "." + v else v)
          if ((vars ++ vars_extended).intersect(unprotected_resource).length>0){
            resource = Some(ctx)
            static_ctx = static_mthd
          }
        }
      }
      case InsertDeclareAndInst(_,cls,line,_,_,_) =>  {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)){
          resource = Some(ctx)
          static_ctx = static_mthd
        }
      }
      case Test(line) => {
        if (Globals.getRealLineNo(ctx.start.getLine) <= line && line <= Globals.getRealLineNo(ctx.stop.getLine)) {
          static_ctx = static_mthd
        }
      }
      case _ => this.visitChildren(ctx)
    }
  }

  override def visitExpressionName(ctx: Java8Parser.ExpressionNameContext): Unit = {
    //println("VISIT EXPRESSION " + ctx.getText)
 //    println(ctx.children)
    visitCriticalSection(ctx)
  }

  /* capture the inner most statement which contains the culprit resource */
  override def visitStatement(ctx: Java8Parser.StatementContext): Unit = {
    resource match {
      case None      => { this.visitChildren(ctx)
        resource match {
          case None =>
          case Some(_) =>
            resource = None
            resourceStatement = Some(ctx)
        }}
      case Some(_) =>
    }
  }

  override def visitClassDeclaration(ctx: Java8Parser.ClassDeclarationContext): Unit = {
    fix match {
      case InsertDeclareAndInst(_,cls,line,_,_,_) =>  {
        val classes = RacerDAPI.classToListOfCls(cls)
        if (classes.contains(ctx.normalClassDeclaration().Identifier().getText)){
          classStmt = Some(ctx)
        }
      }
      case _ => this.visitChildren(ctx)
    }
  }

  override def visitFieldAccess(ctx: Java8Parser.FieldAccessContext): Unit = {
    //println("Field Access" + ctx.getText)
    visitCriticalSection(ctx)
  }

  override def visitFieldAccess_lfno_primary(ctx: Java8Parser.FieldAccess_lfno_primaryContext): Unit = {
    //println("Field Access_lfno_primary" + ctx.getText)
    visitCriticalSection(ctx)
  }

  override def visitFieldAccess_lf_primary(ctx: Java8Parser.FieldAccess_lf_primaryContext): Unit = {
    //println("Field Access_lf_primary" + ctx.getText)
    visitCriticalSection(ctx)
  }

  override def visitResource(ctx: Java8Parser.ResourceContext): Unit = {
    this.visitChildren(ctx)
//    println("Resource" + ctx.getText)
  }

  override def visitMethodInvocation(ctx: Java8Parser.MethodInvocationContext): Unit = {
    visitCriticalSection(ctx)
  }

  override def visitMethodInvocation_lf_primary(ctx: Java8Parser.MethodInvocation_lf_primaryContext): Unit = {
    visitCriticalSection(ctx)
  }

  override def visitMethodInvocation_lfno_primary(ctx: Java8Parser.MethodInvocation_lfno_primaryContext): Unit = {
    visitCriticalSection(ctx)
  }

  override def visitMethodDeclaration(ctx: Java8Parser.MethodDeclarationContext): Unit = {
    val static_prev = static_mthd
    val isStatic    = ctx.methodModifier().toArray.exists(x => x.asInstanceOf[Java8Parser.MethodModifierContext].getText == "static")
    if (isStatic) static_mthd = true
    this.visitChildren(ctx)
    if (isStatic) static_mthd = static_prev
  }

  override def visitVariableInitializer(ctx: Java8Parser.VariableInitializerContext): Unit = {
    //println("Variable Initializer: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitFieldDeclaration(ctx: Java8Parser.FieldDeclarationContext): Unit =  {
    //println("Field Declarator: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitVariableDeclarator(ctx: Java8Parser.VariableDeclaratorContext): Unit = {
    //println("Variable Declarator: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitLocalVariableDeclaration(ctx: Java8Parser.LocalVariableDeclarationContext): Unit = {
   // println("visit visitLocalVariableDeclaration: " + ctx.getText)
    val lst = ctx.variableDeclaratorList().variableDeclarator()
   // println("visit visitLocalVariableDeclaration - variabledeclarator: " + lst)
    this.visitChildren(ctx)
  }

  override def visitLocalVariableDeclarationStatement(ctx: Java8Parser.LocalVariableDeclarationStatementContext): Unit = {
//    println("visit visitLocalVariableStatement: " + ctx.getText)
//    println("Type:" + ctx.localVariableDeclaration().unannType().getText)
//    println("Declarator List:" + ctx.localVariableDeclaration().variableDeclaratorList())
//    println("Declarator List modifiers:" + ctx.localVariableDeclaration().variableModifier())
    this.visitChildren(ctx)
  }

  override def visitAdditiveExpression(ctx: Java8Parser.AdditiveExpressionContext): Unit = {
 //   println("Additive Expression: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitMultiplicativeExpression(ctx: Java8Parser.MultiplicativeExpressionContext): Unit = {
//    println("Multiplicative Expression: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitUnaryExpression(ctx: Java8Parser.UnaryExpressionContext): Unit = {
 //   println("Unary Expression: " + ctx.getText)
    this.visitChildren(ctx)
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
    val textToInsert = fix.modifiers.foldLeft("")((acc,a) => acc + " " + a) + " " + fix.typ + " " + fix.variable + " = " + " new " + fix.typ + "(); "
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
