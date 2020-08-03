package org.racerdfix

import org.racerdfix.language.PrettyPrinting

case class FixConfig(
       // Modus Operandi params
       interactive: Boolean      = false,
       testing:     Boolean      = false

  ) extends PrettyPrinting {

  override def pp: String =
    ( (List(s"interactive = $interactive"))
      ++ (List(s"testing = $testing"))
      ).mkString(", ")
}

case class RacerDFixException(msg: String) extends Exception(msg)