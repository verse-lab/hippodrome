package org.racerdfix

import language.PatchCost
import org.racerdfix.utils.FileManipulation

object Globals {

  /**
   *  There is a discrepancy between the line number returned by RacerDFix (the real line number)
   *  and that returned by antlr which if off by 1 (upwards). To relate to the real line number
   *  we must step up by 1 whatever is indicated by RacerDFix.
   */
  def getRealLineNo(lineno: Int) = lineno - 1
  def getAntlrLineNo(lineno: Int)= lineno + 1
  def getTextOpt[A](par: Option[A]): String = par.toString

  /* Tool's versioning */
  val TOOLNAME          = "RacerDFix"
  val SCRIPTNAME        = "racerdfix"
  val VERSION           = "0.1"
  val VERSION_STRING    = s"v$VERSION"

  /* Cost related values */
  val defCostValue              = 0
  val defUnitValue              = 0
  val maxCostValue              = Int.MaxValue
  val maxCost                   = new PatchCost(maxCostValue)
  val defCost                   = new PatchCost(defCostValue)
  val unitCost                  = new PatchCost(defUnitValue)

  /* default files and paths */
  val results_out_dir           = "src/main/misc/infer-out/"
  val json_bugs_filename        = "report.json"
  val json_summaries_dir        = "racerdfix_summaries"
  val json_patches_filename     = "racerdfix_patches.json"
  val fm  = new FileManipulation
  val json_bugs_file            = fm.getFile(results_out_dir, json_bugs_filename)
  val json_summ_path            = fm.getPath(results_out_dir, json_summaries_dir)
  val json_patches              = fm.getFile(results_out_dir, json_patches_filename)
  val def_src_path              = "src/test/java/"
  val def_src_filename          = "RacyFalseNeg.java"
  val def_src_file              = fm.getFile(def_src_path, def_src_filename)
  val log_file                  = "log.txt"
  val log                       = true

  /* tackled bugs type (see infer's documentation for a full list of bug types) */
  val tackled_bug_kinds         = List("THREAD_SAFETY_VIOLATION_2")

  /* number of iterations for generating patches. If no more bugs are found during one
  *  of the iterations, then the loops stops. */
  val no_iter                   = 5

  /* infer config */
  val def_infer                 = "infer"
  val def_infer_options         = Seq("--racerdfix-only", "--starvation", "--results-dir", results_out_dir)
  val def_target_files          = Seq("--", "javac", def_src_file)
  val config_file               = "CONFIG.json"


}
