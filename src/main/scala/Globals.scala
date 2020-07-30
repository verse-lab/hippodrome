package com.racerdfix

object Globals {

  /**
   *  There is a discrepancy between the line number returned by RacerDFix (the real line number)
   *  and that returned by antlr which if off by 1 (upwards). To relate to the real line number
   *  we must step up by 1 whatever is indicated by RacerDFix.
   */
  def getRealLineNo(lineno: Int) = lineno - 1
  def getAntlrLineNo(lineno: Int)= lineno + 1
  def getTextOpt[A](par: Option[A]): String = par.toString
}
