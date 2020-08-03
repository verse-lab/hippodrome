package org.racerdfix.fixdsl

import org.racerdfix.antlr.Java8Parser
import org.antlr.v4.runtime.{Token, TokenStream, TokenStreamRewriter}

/* FIXES */

sealed trait FixKind
case object NoFix extends FixKind
case class Update(cls: String, line: Int, lock_old: String, lock_new: String) extends FixKind
case class Insert(cls: String, line: Int, resource: String, lock: String) extends FixKind

class PatchCost(val cost: Int) extends Ordered[PatchCost] {
  def compare(that: PatchCost) = this.cost - that.cost
  def add(that: PatchCost) = new PatchCost(this.cost + that.cost)
}

class PatchBlock(val patch: String, val start: Token, val stop: Token, val description: String, val cost: PatchCost) {
  override def toString() : String = {
    patch
  }
}


class Fix(file: String, cls: String, line_start: Int, lines_top: Int, code: String)

/*  BUGS */

sealed trait AccessKind
case object Read extends AccessKind
case object Write extends AccessKind

class FileModif(val filename: String, val rewriter: TokenStreamRewriter)

/* raw snapshot */
class CSumm(val filename: String, val cls: String, val resource: String, val access: AccessKind, val lock: List[String], val line: Int)
/* snapshot */
class Summ(val fm:FileModif, val tree: Java8Parser.CompilationUnitContext, val tokens: TokenStream, val csumm: CSumm)
class Bug(val class1: String, val statement1: CSumm, val class2: String, val statement2: CSumm)