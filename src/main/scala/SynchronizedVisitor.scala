package org.racerdfix

import com.sun.jndi.toolkit.dir.HierMemDirCtx
import org.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext, TokenStreamRewriter}
import org.antlr.v4.runtime.misc.Interval
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.language.{DeclaratorSlicing, FixKind, InsertDeclareAndInst, InsertSync, NoFix, Test, UpdateSync, UpdateVolatile, Variable}

import scala.collection.mutable

class SynchronizedVisitor extends Java8BaseVisitor[Unit] {
  private var fix: FixKind = NoFix
  private var sblock: Option[Java8Parser.SynchronizedStatementContext] = None
  private var resource: Option[Any] = None
  private var targetContext: Option[ParserRuleContext] = None
  private var classStmt: Option[Java8Parser.ClassDeclarationContext] = None
  private var static_mthd: Boolean = false
  private var static_ctx: Boolean = false
  private var decl_slice: Option[DeclaratorSlicing] = None
  var variables: mutable.HashMap[String, List[Variable]] = new mutable.HashMap[String,List[Variable]]()
  private var className: String = ""
  private var config: Option[FixConfig] = None


  def setFix(init_fix: FixKind): Unit = {
    fix = init_fix
  }

  def getSynchronizedBlock = sblock
  def getTargetCtx = targetContext
  def getClassStatement = classStmt
  def getClassStart = classStmt match {
    case Some(cls) => Some (cls.normalClassDeclaration().classBody().start )
    case None => None
  }
  def isStaticCtx = static_ctx
  def getModifiers = if (isStaticCtx) List("static") else Nil
  def getDeclSlice = decl_slice
  def getVariables(cls: String): List[Variable] = {
    variables.getOrElseUpdate(cls,Nil)
  }


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
          //println("LOG INSERT: \n expected vars:" + unprotected_resource + "\n found variables: " + vars_extended)
          if ((vars ++ vars_extended).intersect(unprotected_resource).length>0){
            resource = Some(ctx)
            static_ctx = static_mthd
          } else {
            this.visitChildren(ctx)
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
    //println("VISIT STATEMENT " + ctx.getText)
    resource match {
      case None      => { this.visitChildren(ctx)
        resource match {
          case None =>
          case Some(_) =>
            resource = None
            targetContext = Some(ctx)
        }}
      case Some(_) =>
    }
  }

  override def visitClassDeclaration(ctx: Java8Parser.ClassDeclarationContext): Unit = {
    try {
      val className_prev = className
      className = ctx.normalClassDeclaration().Identifier().getText
      variables.update(className, Nil)
      fix match {
        case InsertDeclareAndInst(_, cls, line, _, _, _) => {
          val classes = RacerDAPI.classToListOfCls(cls)
          /* TODO need to rethink this */
          //val classname_ext = if (className_prev == "") ctx.normalClassDeclaration().Identifier().getText else className_prev + "$" + ctx.normalClassDeclaration().Identifier().getText
          if (classes.contains(ctx.normalClassDeclaration().Identifier().getText) && classStmt == None) {
            println(" Expected: " + cls + ", extended to: " + classes)
            println(" Found: " + ctx.normalClassDeclaration().Identifier().getText + " or  " + className)
            classStmt = Some(ctx)
          }
        }
        case _ =>
      }
      this.visitChildren(ctx)
      className = className_prev
    } catch {
      case _: NullPointerException =>
    }
  }

  override def visitVariableInitializerList(ctx: Java8Parser.VariableInitializerListContext): Unit = {
    visitCriticalSection(ctx)
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
    var modifiers_lst  = List.empty[String]
    var variables_lst  = List.empty[String]
    ctx.fieldModifier().toArray.foreach( m => modifiers_lst = modifiers_lst ++ List(m.asInstanceOf[Java8Parser.FieldModifierContext].getText))
    ctx.variableDeclaratorList().variableDeclarator.forEach( vd => variables_lst = variables_lst ++ List(vd.variableDeclaratorId().Identifier().getText))
    val vars          = variables_lst.map( v => new Variable(modifiers_lst,ctx.unannType().getText,v))
    val existing_vars = variables.getOrElseUpdate(className,Nil)
    variables.update(className,existing_vars ++ vars)
    //println("Field Declarator: " + ctx.getText)
    fix match {
      case UpdateVolatile(fsumm, cls, line, variable, modifiers, decl_old, decl_new) =>
        val classes = RacerDAPI.classToListOfCls(cls)
//        println("LOG VOLATILE: \n expected cls:" + cls + "\n found classes: " + classes)
//        println("\n expected vars:" + variable + "\n found variables: " + variables_lst)
//        println("\n existing modifiers:" + modifiers_lst)
        if (classes.contains(className) && !variables_lst.intersect(variable).isEmpty && !modifiers_lst.contains("volatile")) {
          targetContext = Some(ctx)
          }
      case _ => this.visitChildren(ctx)
    }
  }

  override def visitVariableDeclarator(ctx: Java8Parser.VariableDeclaratorContext): Unit = {
    //println("Variable Declarator: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitLocalVariableDeclaration(ctx: Java8Parser.LocalVariableDeclarationContext): Unit = {
    //println("visit visitLocalVariableDeclaration: " + ctx.getText)
    //val lst = ctx.variableDeclaratorList().variableDeclarator()
    //println("visit visitLocalVariableDeclaration - variabledeclarator: " + lst)
    this.visitChildren(ctx)
  }

  def sliceDeclaratorStatement(ctx: Java8Parser.LocalVariableDeclarationStatementContext): DeclaratorSlicing = {
    var initializers = List.empty[String]
    var modifiers    = List.empty[String]
    var ids          = List.empty[String]
    val typ          = ctx.localVariableDeclaration().unannType().getText
    val variableDeclarator = ctx.localVariableDeclaration().variableDeclaratorList().variableDeclarator()
    ctx.localVariableDeclaration().variableModifier().forEach(m => modifiers = modifiers ++ List(m.getText))
    variableDeclarator.forEach(a => {
      val a0 = a.variableInitializer().start.getStartIndex
      val a1 = a.variableInitializer().stop.getStopIndex
      val interval = new Interval(a0, a1)
      if (!a.variableInitializer().children.isEmpty)  initializers = initializers ++
        List(
          a.variableDeclaratorId().Identifier().getText + " = " +
          a.variableInitializer().start.getInputStream.getText(interval))
    })
//    variableDeclarator.forEach(m => println("Variable declarator id: "  + m.variableDeclaratorId().getText))
//    variableDeclarator.forEach(m => println("Variable declarator id - identifier: "  + m.variableDeclaratorId().Identifier().getText))
    variableDeclarator.forEach(m => ids = ids ++ List(m.variableDeclaratorId().getText))

    val declarations    = Globals.print_list(Globals.pr_id, " ", modifiers) + typ + " " + Globals.print_list(Globals.pr_id, ", ", ids) + "; "
    val initializations = Globals.print_list(Globals.pr_id, "; ", initializers, true)

    //println("TEST:"   + Globals.print_list(Globals.pr_id, " ", modifiers) + typ + " " + Globals.print_list(Globals.pr_id, ", ", ids) + "; " )
    //println("INIT: "  + Globals.print_list(Globals.pr_id, "; ", initializers, true))

    new DeclaratorSlicing(declarations, initializations)
  }

  override def visitLocalVariableDeclarationStatement(ctx: Java8Parser.LocalVariableDeclarationStatementContext): Unit = {
    resource match {
      case None      => { this.visitChildren(ctx)
        resource match {
          case None =>
          case Some(_) =>
            resource = None
            targetContext = Some(ctx)
            decl_slice = Some (sliceDeclaratorStatement(ctx))
        }}
      case Some(_) =>
    }
  }

  override def visitAdditiveExpression(ctx: Java8Parser.AdditiveExpressionContext): Unit = {
    //println("Additive Expression: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitMultiplicativeExpression(ctx: Java8Parser.MultiplicativeExpressionContext): Unit = {
    //println("Multiplicative Expression: " + ctx.getText)
    this.visitChildren(ctx)
  }

  override def visitUnaryExpression(ctx: Java8Parser.UnaryExpressionContext): Unit = {
    //println("Unary Expression: " + ctx.getText)
    this.visitChildren(ctx)
  }

  def insertSynchronizedStatement(rewriter: TokenStreamRewriter,
                                  ctx: ParserRuleContext, fix: InsertSync,
                                  decl_slice: Option[DeclaratorSlicing] = None): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

//    println("ctx:" + ctx.start.getInputStream.getText(interval))

    decl_slice match {
      case None => {
        rewriter.insertBefore(ctx.start, "synchronized(" + fix.lock + ") { ")
        rewriter.insertAfter(ctx.stop, " } ")
      }
      case Some(sdecl) => {
        val decl = sdecl.declarations
        val init = sdecl.initializations
        val new_init = "synchronized(" + fix.lock + ") { " + init + " } "
        rewriter.replace(ctx.start, ctx.stop, decl + " " + new_init)
      }
    }

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

  def updateListOfModifiers(rewriter: TokenStreamRewriter,
                                  ctx: Java8Parser.FieldDeclarationContext, fix: UpdateVolatile): (String,String) = {
    val a = ctx.start.getStartIndex
    val b = ctx.stop.getStopIndex
    val interval = new Interval(a, b)

    val typ       = ctx.unannType()

    var modifiers_str: List[String] = Nil
    ctx.fieldModifier().forEach( m => modifiers_str = modifiers_str ++ List(m.getText))


    var target_field: Option[Java8Parser.VariableDeclaratorContext] = None
    var rest_field: List[Java8Parser.VariableDeclaratorContext]     = Nil
    val fields    = ctx.variableDeclaratorList()
    fields.variableDeclarator().forEach( vd => {
      if (fix.variable.contains(vd.variableDeclaratorId().Identifier().getText)) target_field = Some(vd)
      else rest_field = rest_field ++ List(vd)
    } )

    def string_of_VD (vd: Java8Parser.VariableDeclaratorContext) = {
      val a0 = vd.start.getStartIndex
      val a1 = vd.stop.getStopIndex
      val interval = new Interval(a0, a1)
      vd.start.getInputStream.getText(interval)
    }

    val volatile_declaration = target_field match {
      case None     => ""
      case Some(vd) => {
        Globals.print_list(Globals.pr_id, " ", (modifiers_str ++ List("volatile")).distinct) +
          " " + typ.getText +
          " " + string_of_VD(vd) +
          "; "
      }
    }

    val rest_declarations = rest_field match {
      case Nil => ""
      case _   =>
        Globals.print_list(Globals.pr_id, " ", modifiers_str) +
          " " + typ.getText +
          " " + Globals.print_list(string_of_VD, ", ", rest_field) +
          "; "
    }


    rewriter.replace(ctx.start, ctx.stop, volatile_declaration + rest_declarations)

    //println("VOLATILE: " + volatile_declaration + rest_declarations)

    (ctx.start.getInputStream.getText(interval), volatile_declaration + rest_declarations)
  }

}
