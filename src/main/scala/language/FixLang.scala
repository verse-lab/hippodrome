package org.racerdfix.language

import org.antlr.v4.runtime.{ParserRuleContext, Token, TokenStreamRewriter}
import org.racerdfix.Globals
import org.racerdfix.utils.{ASTStoreElem, RewriterExt}


/* ******************************************************** */
/*                FIX OBJECTS KINDS                         */
/* ******************************************************** */

sealed trait FixKind
case object NoFix extends FixKind
case class Test(lines: Int) extends FixKind
case class UpdateVolatile(val fsumm: FSumm, cls: String, line: Int, variable: Variable, modifiers: List[String], decl_old: String, decl_new: String) extends FixKind
case class UpdateSync(val fsumm: FSumm, cls: String, line: Int, lock_old: Variable, lock_new: Variable) extends FixKind
case class InsertSync(val fsumm: FSumm, cls: String, line: Int, resource: Variable, lock: Variable) extends FixKind
case class InsertDeclare(val fsumm: FSumm, cls: String, line: Int, typ: String, variable: String) extends FixKind
case class MergeFixes(val fixes: List[InsertSync]) extends FixKind
case class MergeTwoInserts(val ins1: InsertSync, val ins2: InsertSync) extends FixKind
case class MergePatchWithInserts(val patch: PatchBlock, val ins2: InsertSync) extends FixKind
case class InsertDeclareAndInst(val fsumm: FSumm, line: Int, variable: Variable, modifiers: List[String]) extends FixKind {
  def clone( fsumm: FSumm = this.fsumm,
             line: Int = this.line,
             variable: Variable = this.variable,
             modifiers: List[String] = this.modifiers) = {
    new InsertDeclareAndInst(fsumm,line,variable,modifiers)
  }
}
case class And(left: FixKind, right: FixKind) extends FixKind {
  def mkAnd(lst: List[FixKind]) = {

    lst match {
      case Nil     => NoFix
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => And(fix,acc))
    }
  }

  def listOf(): List[FixKind] = {
    val left_lst = left match {
      case And(_,_) => left.asInstanceOf[And].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case And(_,_) => right.asInstanceOf[And].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

}

case class Or(left: FixKind, right: FixKind) extends FixKind {

  def mkOr(lst: List[FixKind]) = {
    lst match {
      case Nil     => NoFix
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => Or(fix,acc))
    }
  }

  def listOf(): List[FixKind] = {
     val left_lst = left match {
      case Or(_,_) => left.asInstanceOf[Or].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case Or(_,_) => right.asInstanceOf[Or].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

}

/* ********************************************************** */
/*                                                            */
/* ********************************************************** */

class PatchCost(val cost: Int) extends Ordered[PatchCost] {
  def compare(that: PatchCost) = this.cost - that.cost
  def add(that: PatchCost) = new PatchCost(this.cost + that.cost)
}
sealed trait RewriteKind {
  def getText() = ""
}
case object Replace   extends RewriteKind  { override def getText() = "Replace"}
case object InsBefore extends RewriteKind  { override def getText() = "InsBefore"}
case object InsAfter  extends RewriteKind  { override def getText() = "InsAfter"}

class PatchBlock(var rewriter: RewriterExt, val kind: RewriteKind, val patch: String,
                 val start: Token, val stop: Token, val description: String, val cost: PatchCost,
                 val modifiers: List[String],
                 val sblock : Option[ParserRuleContext] = None) {
  override def toString() : String = {
    patch
  }

  def toStringDetailed() : String = {
    description
  }

  def equals(that: PatchBlock): Boolean = {
    this.kind == that.kind &&
    this.patch == that.patch &&
    this.start == that.start &&
    this.stop == that.stop
  }

  def subsumes(that: PatchBlock): Boolean = {
      this.start.getStartIndex < that.start.getStartIndex &&
      that.stop.getStopIndex <= this.stop.getStopIndex
  }

  def overlaps(that: PatchBlock): Boolean = {
    this.kind == Replace && that.kind == Replace &&
      !(this.stop.getStopIndex < that.start.getStartIndex ||
    that.stop.getStopIndex < this.start.getStartIndex)
  }

}

sealed trait Patch {
  def contains(f: PatchBlock => Boolean): Boolean
}
case object NoPatch extends Patch {
  def contains(f: PatchBlock => Boolean) = false
}
case class PTest(val block: PatchBlock) extends Patch {
  def contains(f: PatchBlock => Boolean) = false
}
case class PInsert(val id: String, val block: PatchBlock) extends Patch {
  def contains(f: PatchBlock => Boolean) = {
      f(this.block)
  }
}
case class PUpdate(val id: String, val block: PatchBlock) extends Patch{
  def contains(f: PatchBlock => Boolean) = {
    f(this.block)
  }
}
case class PAnd(val id: String, val left: Patch, val right: Patch) extends Patch {

  def mkAnd(lst: List[Patch]) = {
    lst match {
      case Nil     => NoPatch
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => PAnd(id,fix,acc))
    }
  }

  def listOf(): List[Patch] = {
    val left_lst = left match {
      case PAnd(_,_,_) => left.asInstanceOf[PAnd].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case PAnd(_,_,_) => right.asInstanceOf[PAnd].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

  def contains(f: PatchBlock => Boolean): Boolean = {
    this.left.contains(f) || this.right.contains(f)
  }
}
case class POr(val id: String, val left: Patch, val right: Patch) extends Patch {

  def mkOr(lst: List[Patch]) = {
    lst match {
      case Nil     => NoPatch
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => POr(id,fix,acc))
    }
  }

  def listOf(): List[Patch] = {
    val left_lst = left match {
      case POr(_,_,_) => left.asInstanceOf[POr].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case POr(_,_,_) => right.asInstanceOf[POr].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

  def contains(f: PatchBlock => Boolean) = {
    this.left.contains(f) || this.right.contains(f)
  }
}

/* represents the choice of the lock to be introduced for complete synchronization */
sealed trait LockChoice{
  def toText() = ""

  def getLockChoice(str: String) = {
    if (str == OccurenceMax.toText()) OccurenceMax
    else if (str == OccurenceMin.toText()) OccurenceMin
    else if (str == NewLock.toText()) NewLock
    else NoLock
  }
}
case object OccurenceMax extends LockChoice {
  override def toText() = "OccurenceMax"
}
case object OccurenceMin extends LockChoice {
  override def toText() = "OccurenceMin"
}
case object NewLock      extends LockChoice {
  override def toText() = "NewLock"
}
case object NoLock extends LockChoice {
override def toText() = "NoLock"
}


class Fix(file: String, cls: String, line_start: Int, lines_top: Int, code: String)

/*  BUGS */

sealed trait AccessKind
case object Read extends AccessKind
case object Write extends AccessKind
case object Unk extends AccessKind

sealed trait Trace {
  def length() = {
    this match {
      case EmptyTrace => 0
      case NonEmptyTrace(trace) => trace.length
    }
  }
  def equals(that: Trace): Boolean = {
    (this,that) match {
      case (EmptyTrace,EmptyTrace) => true
      case (NonEmptyTrace(trace1), NonEmptyTrace(trace2)) =>
        Globals.eq_list(((a:TraceElemShort,b: TraceElemShort) => a.equals(b)), trace1, trace2)
      case (_,_) => false
    }
  }
}
case object EmptyTrace extends Trace
case class  NonEmptyTrace(val trace:List[TraceElemShort]) extends Trace

class Lock(val obj: String, val cls: String, val resource: Variable) {
  def equals(obj: Lock): Boolean = this.obj == obj.obj && this.cls == obj.cls && this.resource.equals_loose(obj.resource)
}

/* raw racerdfix snapshot */
class RFSumm(val filename: String, val cls: String, val procedure: String, val resource: Variable, val access: AccessKind, val locks: List[Lock], val line: Int, val trace: Trace, val hash: String){

  def getCost(cost: RFSumm => Int ) = cost(this)

  def equals(that: RFSumm) = {
    this.filename == that.filename &&
    this.cls      == that.cls &&
    this.procedure== that.procedure &&
    this.resource.equals(that.resource) &&
    this.access   == that.access &&
    Globals.eq_set(((a:Lock, b: Lock) => a.equals(b)), this.locks, that.locks) &&
    this.line     == that.line &&
    this.trace.equals(that.trace) &&
    this.hash     == that.hash
  }

  def equals_weak(that: RFSumm) = {
    this.filename == that.filename &&
      this.cls      == that.cls &&
      this.procedure== that.procedure &&
      this.resource.equals_weak(that.resource) &&
      this.access   == that.access &&
      Globals.eq_set(((a:Lock, b: Lock) => a.equals(b)), this.locks, that.locks) &&
      this.line     == that.line
  }
}

/* racerdfix snapshot */
class FSumm(var ast: ASTStoreElem, val csumm: RFSumm)
/* racerdfix bug */
class FBug(val file: String, val cls: String, val proc: String,
           val bug_trace: List[TraceElem],
           val snapshot1: List[RFSumm], val snapshot2: List[RFSumm], val hash: String) {

  def equals_weak(that: FBug) = {
    this.file == that.file &&
    this.cls  == that.cls  &&
    this.proc == that.proc &&
//      Globals.eq_set(((a:TraceElem, b: TraceElem) => a.equals(b)), this.bug_trace, that.bug_trace) &&
      Globals.eq_set(((a:RFSumm, b: RFSumm) => a.equals_weak(b)), this.snapshot1, that.snapshot1)
//      Globals.eq_set(((a:RFSumm, b: RFSumm) => a.equals_weak(b)), this.snapshot2, that.snapshot2)
  }
}

/**/
class DeclaratorSlicing(val declarations: String, val initializations: String)

class Variable(val cls: String, val modifiers: List[String] = Nil, val typ: String = "", val id: String, val aliases: List[String] = Nil){
  def isStatic() = {
    this.modifiers.contains("static")
  }

  def equals(that: Variable): Boolean = {
    this.cls == that.cls &&
    Globals.eq_set(((a:String,b:String) => a == b), this.modifiers, that.modifiers) &&
    this.typ == that.typ &&
    this.id  == that.id  &&
    Globals.eq_set(((a:String,b:String) => a == b), this.id::this.aliases, that.id::that.aliases)
  }

  def equals_loose(that: Variable): Boolean = {
    this.cls == that.cls &&
      this.id  == that.id  &&
      Globals.eq_set(((a:String,b:String) => a == b), this.id::this.aliases, that.id::that.aliases)
  }

  def equals_weak(that: Variable): Boolean = {
    this.cls == that.cls &&
      (this.id.startsWith(that.id) || that.id.startsWith(this.id))
  }

  def isIn(lst : List[Variable]) : Boolean = {
    lst.contains((v:Variable) => this.equals(v))
  }

  def allAliases(): List[String] = {
    (this.id :: this.aliases).distinct
  }

  override def toString(): String = {
    "Variable("  + this.cls + ", " + typ  + " , " + id + " , {" +  Globals.print_list(Globals.pr_id,", ", this.aliases ) + "}"
  }
}