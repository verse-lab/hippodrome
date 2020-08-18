package org.racerdfix

import org.racerdfix.language.PrettyPrinting

case class FixConfig(
                      // Modus Operandi params
                      interactive: Boolean      = false,
                      testing:     Boolean      = false,
                      intellij:    Boolean      = false,
                      log:         Boolean      = true,
                      // Files
                      json_path: String         = Globals.results_out_dir,
                      json_bugs:   String       = Globals.json_bugs_file,
                      json_summaries: String    = Globals.json_summaries,
                      json_patches: String      = Globals.json_patches,
                      java_sources_path: String = Globals.def_src_path,
                      log_file: String          = Globals.log_file,
                      // Infer
                      config_file: String       = Globals.config_file,
                      infer: String             = Globals.def_infer,
                      infer_opt: Seq[String]    = Globals.def_infer_options,
                      infer_target_files: Seq[String]  = Globals.def_target_files,
                      prio_files: List[String]  = Nil  // checks only the bugs in prio_files if the list in non-empty

  ) extends PrettyPrinting {

  override def pp: String =
    ( (List(s"interactive = $interactive"))
      ++ (List(s"testing = $testing"))
      ).mkString(", ")

  def getJsonBugsResults = {
    if (json_patches != Globals.json_patches) json_patches
    else json_path + Globals.json_patches_filename
  }

  def getJsonBugs = {
    if (json_bugs != Globals.json_bugs_file) json_bugs
    else json_path + Globals.json_bugs_filename
  }

  def getJsonSummaries = {
    if (json_summaries != Globals.json_summaries) json_summaries
    else json_path + Globals.json_summaries_filename
  }
}

case class RacerDFixException(msg: String) extends Exception(msg)

class Config(val infer: String,
             val infer_opt: Seq[String],
             val json_path: String,
             val infer_target_files: Seq[String],
             val prio_files: List[String]
            )