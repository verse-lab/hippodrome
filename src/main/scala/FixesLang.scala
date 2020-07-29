package com.racerdfix.fixdsl

/* FIXES */

sealed trait FixKind
case object NoFix extends FixKind
case class Update(cls: String, line: Int, lock_old: String, lock_new: String) extends FixKind
case class Insert(cls: String, line: Int, lock: String) extends FixKind

class Patch(code: String)

class Fix(file: String, cls: String, line_start: Int, lines_top: Int, code: String)

/*  BUGS */

sealed trait AccessKind
case object Read extends AccessKind
case object Write extends AccessKind

class CSumm(var resource: String, var access: AccessKind, var lock: List[String], var line: Int)
class Bug(var class1: String, var statement1: CSumm, var class2: String, var statement2: CSumm)