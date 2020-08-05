package org.racerdfix

import org.racerdfix.language.PrettyPrinting

case class FixConfig(
       // Modus Operandi params
       interactive: Boolean      = false,
       testing:     Boolean      = false,
       // Files
       json_bugs:   String       = Globals.json_bugs_file,
       json_summaries: String    = Globals.json_summaries,
       json_patches: String      = Globals.json_patches

  ) extends PrettyPrinting {

  override def pp: String =
    ( (List(s"interactive = $interactive"))
      ++ (List(s"testing = $testing"))
      ).mkString(", ")
}

case class RacerDFixException(msg: String) extends Exception(msg)