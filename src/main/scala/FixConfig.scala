package org.racerdfix

import org.racerdfix.language.PrettyPrinting

case class FixConfig(
                      // Modus Operandi params
                      interactive: Boolean      = false,
                      testing:     Boolean      = false,
                      // Files
                      json_path: String         = Globals.results_out_dir,
                      json_bugs:   String       = Globals.json_bugs_file,
                      json_summaries: String    = Globals.json_summaries,
                      json_patches: String      = Globals.json_patches,
                      java_sources_path: String = Globals.def_src_path,
                      // Infer
                      infer: String             = Globals.def_infer,
                      infer_opt: Seq[String]    = Globals.def_infer_options,
                      infer_target_files: Seq[String]  = Globals.def_target_files

  ) extends PrettyPrinting {

  override def pp: String =
    ( (List(s"interactive = $interactive"))
      ++ (List(s"testing = $testing"))
      ).mkString(", ")
}

case class RacerDFixException(msg: String) extends Exception(msg)