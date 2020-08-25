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
  val defCostValue              = 1
  val defUnitValue              = 1
  val maxCostValue              = Int.MaxValue
  val defVolatileValue          = maxCostValue
  val maxCost                   = new PatchCost(maxCostValue)
  val defCost                   = new PatchCost(defCostValue)
  val unitCost                  = new PatchCost(defUnitValue)
  val volatileCost              = new PatchCost(defVolatileValue)

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

  /* MISC */
  val base_obj_id = "objR"
  val def_obj_typ = "Object"

  def print_list[A](printer: A => String, separator: String, lst: List[A], last: Boolean = false) = {
    def helper(lst: List[A]): String =
      lst match {
        case Nil    => ""
        case x::Nil => printer(x) + (if(last) separator else "")
        case x::xs  => printer(x) + separator + helper(xs)
      }
    helper(lst)
  }

  def pr_id(str: String): String = str

  def eq_list[A](eq_fn: (A,A) => Boolean, lst1: List[A], lst2: List[A]) : Boolean = {
    if(!(lst1.length == lst2.length)) false
    else lst1.zip(lst2).forall( a => eq_fn(a._1,a._2))
  }

  def contains_eq[A](eq: (A,A) => Boolean, lst: List[A], elem: A): Boolean = {
    lst.exists( a => eq(a, elem))
  }

  def eq_set[A](eq: (A,A) => Boolean, lst1: List[A], lst2: List[A]) : Boolean = {
    lst1.forall( a => contains_eq(eq, lst2, a)) &&
    lst2.forall( a => contains_eq(eq, lst1, a))
  }

  def distinct_eq[A](eq: (A,A) => Boolean, lst: List[A]) : List[A] = {
    lst.foldLeft(List[A]())((acc,elem) => if (contains_eq(eq,acc,elem)) acc else acc ++ List(elem))
  }

}
