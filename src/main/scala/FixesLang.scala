package com.racerdfix.fixdsl

sealed trait Cls {}
sealed trait Lock

sealed trait FixKind
case object NoFix extends FixKind
case class Update(cls: String, line: Int, lock_old: String, lock_new: String) extends FixKind
case class Insert(cls: String, line: Int, lock: String) extends FixKind

class Patch(code: String)

class Fix(var file: String, var cls: String, line_start: Int, lines_top: Int, code: String)

