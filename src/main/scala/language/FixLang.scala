package org.racerdfix.language

import org.antlr.v4.runtime.{Token, TokenStream, TokenStreamRewriter}
import org.racerdfix.antlr.Java8Parser
import org.racerdfix.utils.{ASTStoreElem, FileModif}

/* FIXES */

sealed trait FixKind
case object NoFix extends FixKind
case class Update(cls: String, line: Int, lock_old: String, lock_new: String) extends FixKind
case class Insert(cls: String, line: Int, resource: String, lock: String) extends FixKind
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


class PatchCost(val cost: Int) extends Ordered[PatchCost] {
  def compare(that: PatchCost) = this.cost - that.cost
  def add(that: PatchCost) = new PatchCost(this.cost + that.cost)
}

class PatchBlock(var rewriter: TokenStreamRewriter, val patch: String, val start: Token, val stop: Token, val description: String, val cost: PatchCost) {
  override def toString() : String = {
    patch
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

class Lock(val obj: String, val cls: String, val resource: String)

/* raw racerdfix snapshot */
class RFSumm(val filename: String, val cls: String, val resource: String, val access: AccessKind, val locks: List[Lock], val line: Int, val trace: Trace, val hash: String){
  def getCost(cost: RFSumm => Int ) = cost(this)
}
/* racerdfix snapshot */
class FSumm(var ast: ASTStoreElem, val csumm: RFSumm)
/* racerdfix bug */
class FBug(val snapshot1: List[RFSumm], val snapshot2: List[RFSumm], val hash: String)