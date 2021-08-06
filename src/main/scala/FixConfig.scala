package org.racerdfix

import org.racerdfix.inferAPI.InterpretJson
import org.racerdfix.language.{LockChoice, NewLock, NoLock, OccurenceMax, OccurenceMin, PrettyPrinting}
import org.racerdfix.utils.FileManipulation


class ToolConfig(
                 val infer: String,            // path to infer
                 val infer_opt: Seq[String],   // the options infer is executed with
                 val json_path: String,        // path to the files in which infer stores its summaries and reports
            )

class Config(val infer: String,
             val infer_opt: Seq[String],
             val json_path: String,
             val infer_target_files: Seq[String],
             val prio_files: List[String],
             val iterations: Int,
             val hippodrome_options: Seq[String],
            )
case class RunConfig(fixConfig: FixConfig, fileName: String)

case class FixConfig(
                      // Modus Operandi params
                      interactive: Boolean      = false,
                      testing:     Boolean      = false,
                      intellij:    Boolean      = false,
                      log:         Boolean      = true,
                      iterations:  Int          = Globals.no_iter,
                      atomicity:   Boolean      = false,
                      lock_choice: LockChoice   = Globals.lockCost,
                      // Files
                      json_path: String         = Globals.results_out_dir,
                      json_bugs:   String       = Globals.json_bugs_file,
                      json_summaries: String    = Globals.json_summ_path,
                      json_patches: String      = Globals.json_patches,
                      java_sources_path: String = Globals.def_src_path,
                      log_file: String          = Globals.log_file,
                      // Infer
                      target_config_file: String= Globals.target_config_file,
                      tools_config_file:  String= Globals.tools_config_file,
                      infer: String             = Globals.def_infer,
                      infer_opt: Seq[String]    = Globals.def_infer_options,
                      infer_target_files: Seq[String]  = Globals.def_target_files,
                      prio_files: List[String]  = Nil, // checks only the bugs in prio_files if the list in non-empty
                      // General purpose flags
                      flag1: Boolean            = false,
                      flag2: Boolean            = false,
                      config_options: Seq[String] = Seq.empty
  ) extends PrettyPrinting {

  val fm = new FileManipulation

  override def pp: String =
    ( (List(s"interactive = $interactive"))
      ++ (List(s"testing = $testing"))
      ).mkString(", ")

  def getJsonBugsResults = {
    if (json_patches != Globals.json_patches) json_patches
    else fm.getPath(json_path, Globals.json_patches_filename)
  }

  def getJsonBugs = {
    if (json_bugs != Globals.json_bugs_file) json_bugs
    else fm.getFile(json_path, Globals.json_bugs_filename)
  }

  def getJsonSummariesPath = {
    if (json_summaries != Globals.json_summ_path) json_summaries
    else fm.getPath(json_path,Globals.json_summaries_dir)
  }

}

object ArgParser {
  def argsParser =  new {

  } with scopt.OptionParser[RunConfig](Globals.SCRIPTNAME) {
    opt[String]("fileName").action {(x, c) =>
      c.copy(fileName = x)
    }.text("a synthesis file name (the file under the specified folder, called filename.syn)")

    opt[Boolean]('i', "interactive").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(interactive = b))
    }.text("runs " + Globals.TOOLNAME + " in interactive mode - the user is expected to choose a patch")

    opt[Boolean]("testing").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(testing = b))
    }.text("runs " + Globals.TOOLNAME + " in testing mode - generated fixes do not overwrite the original file")

    opt[Boolean]( "intellij").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(intellij = b))
    }.text("runs " + Globals.TOOLNAME + " in IntelliJ mode - runs infer only once")

    opt[Boolean]( "atomicity").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(atomicity = b))
    }.text("aims to fix atomicity violations [in Beta]")

    opt[Int]("interations").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(iterations = b))
    }.text("the number of times " + Globals.TOOLNAME + " attempts to create patches until all the bugs are solved. The default value is " + Globals.no_iter)

    opt[Boolean]('l', "log").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(log = b))
    }.text("logs all the applied patches")

    opt[String]("json_bugs").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_bugs = b))
    }.text("sets the file which provides the bug details. The default one is " + Globals.json_bugs_file)

    opt[String]("json_summaries").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_summaries = b))
    }.text("sets the file which provides the methods' summaries. The default one is " + Globals.json_summ_path)

    opt[String]("json_patches").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_patches = b))
    }.text("sets the file where the generated patches will be stored. The default one is " + Globals.json_patches)

    opt[String]("log_file").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_patches = b))
    }.text("sets the logging. The default one is " + Globals.log_file)

    opt[String]('j', "java_sources_path").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(java_sources_path = b))
    }.text("the path to the source files. The default one is " + Globals.def_src_path)

    opt[String]("lock_choice").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(lock_choice = NewLock.getLockChoice(b)))
    }.text("decides how the candidate lock is chosen. The default one is " + Globals.lockCost.toText()
      + " Choices:{"
      + Globals.print_list_def[LockChoice]((x => x.toText()), List(OccurenceMin,OccurenceMax,NewLock))
      + " }")

    opt[String]("infer").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer = b))
    }.text("the infer process to be called for generating json and validating. The default one is " + Globals.def_infer)

    opt[Seq[String]]("infer_opt").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer_opt = b))
    }.text("the options infer runs with. The default one is " + Globals.def_infer_options)

    opt[Seq[String]]('f', "infer_target_files").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer_target_files = b))
    }.text("the target files to analyse and fix. The default one is " + Globals.def_target_files)

    opt[Boolean]("flag1").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(flag1 = b))
    }.text("Flag used for debugging. Default value is " + false)

    opt[Boolean]("flag2").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(flag2 = b))
    }.text("Flag used for debugging. Default value is " + false)

    opt[String]('c', "config_file").action { (b, rc) =>
      rc.copy(fixConfig = {
        val jsonTranslator = new InterpretJson(rc.fixConfig.copy(target_config_file = b))
        val config = jsonTranslator.getJsonConfig()
        rc.fixConfig.copy(target_config_file = b,
          infer     = config.infer,
          infer_opt = config.infer_opt,
          infer_target_files = config.infer_target_files,
          json_path  = config.json_path,
          prio_files = config.prio_files,
          iterations = config.iterations,
          config_options = config.hippodrome_options
        )
      })
    }.text("the file with repair config (target file, iterations, etc). The default one is " + Globals.target_config_file)

    help("help").text("prints this usage text")
  }
}

case class RacerDFixException(msg: String) extends Exception(msg)

