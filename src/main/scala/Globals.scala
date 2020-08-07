package org.racerdfix

import language.PatchCost

object Globals {

  /**
   *  There is a discrepancy between the line number returned by RacerDFix (the real line number)
   *  and that returned by antlr which if off by 1 (upwards). To relate to the real line number
   *  we must step up by 1 whatever is indicated by RacerDFix.
   */
  def getRealLineNo(lineno: Int) = lineno - 1
  def getAntlrLineNo(lineno: Int)= lineno + 1
  def getTextOpt[A](par: Option[A]): String = par.toString

  /* Cost related values */
  val defCostValue = 0
  val defUnitValue = 0
  val maxCostValue = Int.MaxValue
  val maxCost      = new PatchCost(maxCostValue)
  val defCost      = new PatchCost(defCostValue)
  val unitCost     = new PatchCost(defUnitValue)

  /* default files and paths */
  val results_out_dir           = "src/main/misc/infer-out/"
  val json_files_path           = "src/main/misc/infer-out/"
  val json_bugs_filename        = "report.json"
  val json_summaries_filename   = "racerdfix_summaries.json"
  val json_patches_filename     =  "racerdfix_patches.json"
  val json_bugs_file   = json_files_path + json_bugs_filename
  val json_summaries   = json_files_path + json_summaries_filename
  val json_patches     = json_files_path + json_patches_filename
  val def_src_path     = "src/test/java/"
  val def_src_filename = "RacyFalseNeg.java"
  val def_src_file     = def_src_path + def_src_filename


}
