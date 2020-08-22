package org.racerdfix.language

import org.antlr.v4.runtime.{Token, TokenStreamRewriter}
import org.racerdfix.utils.{ASTStoreElem}


/* ******************************************************** */
/*                FIX OBJECTS KINDS                         */
/* ******************************************************** */

sealed trait FixKind
case object NoFix extends FixKind
case class Test(lines: Int) extends FixKind
case class UpdateSync(val fsumm: FSumm, cls: String, line: Int, lock_old: String, lock_new: String) extends FixKind
case class InsertSync(val fsumm: FSumm, cls: String, line: Int, resource: List[String], lock: String) extends FixKind
case class InsertDeclare(val fsumm: FSumm, cls: String, line: Int, typ: String, variable: String) extends FixKind
case class InsertDeclareAndInst(val fsumm: FSumm, cls: String, line: Int, typ: String, variable: String, modifiers: List[String]) extends FixKind {
  def clone( fsumm: FSumm = this.fsumm,
             cls: String = this.cls,
             line: Int = this.line,
             typ: String = this.typ,
             variable: String = this.variable,
             modifiers: List[String] = this.modifiers) = {
    new InsertDeclareAndInst(fsumm,cls,line,typ,variable,modifiers)
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

class PatchBlock(var rewriter: TokenStreamRewriter, val kind: RewriteKind, val patch: String,
                 val start: Token, val stop: Token, val description: String, val cost: PatchCost,
                 val modifiers: List[String]) {
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
      that.stop.getStopIndex < this.stop.getStopIndex
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
case class PInsert(val id: Int, val block: PatchBlock) extends Patch {
  def contains(f: PatchBlock => Boolean) = {
      f(this.block)
  }
}
case class PUpdate(val id: Int, val block: PatchBlock) extends Patch{
  def contains(f: PatchBlock => Boolean) = {
    f(this.block)
  }
}
case class PAnd(val left: Patch, val right: Patch) extends Patch {

  def mkAnd(lst: List[Patch]) = {
    lst match {
      case Nil     => NoPatch
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => PAnd(fix,acc))
    }
  }

  def listOf(): List[Patch] = {
    val left_lst = left match {
      case PAnd(_,_) => left.asInstanceOf[PAnd].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case PAnd(_,_) => right.asInstanceOf[PAnd].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

  def contains(f: PatchBlock => Boolean): Boolean = {
    this.left.contains(f) || this.right.contains(f)
  }
}
case class POr(val left: Patch, val right: Patch) extends Patch {

  def mkOr(lst: List[Patch]) = {
    lst match {
      case Nil     => NoPatch
      case x::Nil  => x
      case _       => lst.dropRight(1).foldRight(lst.last)((fix,acc) => POr(fix,acc))
    }
  }

  def listOf(): List[Patch] = {
    val left_lst = left match {
      case POr(_,_) => left.asInstanceOf[POr].listOf()
      case _ => List(left)
    }
    val right_lst = right match {
      case POr(_,_) => right.asInstanceOf[POr].listOf()
      case _ => List(right)
    }
    left_lst ++ right_lst
  }

  def contains(f: PatchBlock => Boolean) = {
    this.left.contains(f) || this.right.contains(f)
  }
}



class Fix(file: String, cls: String, line_start: Int, lines_top: Int, code: String)

/*  BUGS */

sealed trait AccessKind
case object Read extends AccessKind
case object Write extends AccessKind

sealed trait Trace {
  def length() = {
    this match {
      case EmptyTrace => 0
      case NonEmptyTrace(trace) => trace.length
    }
  }
}
case object EmptyTrace extends Trace
case class  NonEmptyTrace(val trace:List[String]) extends Trace

class Lock(val obj: String, val cls: String, val resource: String) {
  def equals(obj: Lock): Boolean = this.obj == obj.obj && this.cls == obj.cls && this.resource == obj.resource
}

/* raw racerdfix snapshot */
class RFSumm(val filename: String, val cls: String, val resource: List[String], val access: AccessKind, val locks: List[Lock], val line: Int, val trace: Trace, val hash: String){
  def getCost(cost: RFSumm => Int ) = cost(this)
}
/* racerdfix snapshot */
class FSumm(var ast: ASTStoreElem, val csumm: RFSumm)
/* racerdfix bug */
class FBug(val snapshot1: List[RFSumm], val snapshot2: List[RFSumm], val hash: String)

/**/
class DeclaratorSlicing(val declarations: String, val initializations: String)