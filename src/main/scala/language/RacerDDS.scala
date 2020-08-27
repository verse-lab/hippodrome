package  org.racerdfix.language

import org.racerdfix.{Globals, GroupByIdPatchOptions}
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.utils.PatchStore

import scala.collection.mutable.HashMap

/*  bug */
class TraceElem(val level: Int, val filename: String, val line_number: Int,val column_number: Int, val description: String)

/* summary */
class Access(val kind: String, val exp: String)
class Elem(val access: Access,val thread: String,val locks: List[String],val ownership_pre: String)
class AccessElem(val elem: Elem, val loc: Int, val trace: List[String], val hash: String)

/* RacerD's data structures*/
sealed trait Bugs
case object OtherBug extends Bugs
case class BugIn(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
            val proc: String, val proc_start: Int, val file: String,
            val bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String, val access: String,
            val snapshot1_hash: Option[String], val snapshot2_hash: Option[String]) extends  Bugs {

  def racerDToRacerDFix(summaries: List[RFSumm]): FBug = {
    val summ1 = snapshot1_hash match {
      case None => Nil
      case Some(str)  => summaries.filter(sum => sum.hash == str)
    }
    val summ2 = snapshot2_hash match {
      case None => Nil
      case Some(str)  => summaries.filter(sum => sum.hash == str)
    }
    /*
    class FBug(val file: String, val cls: String, val proc: String,
               val bug_trace: List[TraceElem],
           val snapshot1: List[RFSumm], val snapshot2: List[RFSumm], val hash: String)

    * */
    new FBug(
      file,
      RacerDAPI.classNameOfMethodString(proc),
      RacerDAPI.procedureOfString(proc),
      bug_trace,
      Globals.distinct_eq( (a:RFSumm,b:RFSumm) => a.equals(b), summ1),
      Globals.distinct_eq( (a:RFSumm,b:RFSumm) => a.equals(b), summ2),
      hash)
  }

  def toBugOut(patchStore: PatchStore): BugOut = {
    try {
      new BugOut(bug_type,qualifier,severity, line, column, proc, proc_start, file, bug_trace, key, hash, bug_type_hum, access,
        snapshot1_hash, snapshot2_hash, patchStore.map(hash).choiceId, patchStore.map(hash).patches)
    } catch {
      case _ =>        new BugOut(bug_type,qualifier,severity, line, column, proc, proc_start, file, bug_trace, key, hash, bug_type_hum, access,
        snapshot1_hash, snapshot2_hash, "-1", new GroupByIdPatchOptions(HashMap.empty))
    }
  }
}

case class BugOut(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
                   val proc: String, val proc_start: Int, val file: String,
                   val bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String, val access: String,
                   val snapshot1_hash: Option[String], val snapshot2_hash: Option[String],
                   val patch_choice: String,
                   val patches: GroupByIdPatchOptions) extends Bugs


class SummaryIn(var file: String, val procedure: String, var accesses: List[AccessElem]) {

  def updateAccesses(accesses: List[AccessElem]) = {
    this.accesses = accesses
  }

  def updatePath(path: String) = {
    this.file = path + this.file
  }

  def updatePathReturn(path: String) = {
    updatePath(path)
    this
  }

  def racerDToRacerDFix(): List[RFSumm] = {
    this.accesses.foldLeft(List.empty[RFSumm])((acc2, ae) => {
        val cls      = RacerDAPI.classNameOfMethodString(procedure)
        val proc     = RacerDAPI.procedureOfString(procedure)
        val resource = RacerDAPI.varOfResource(ae.elem.access.exp,cls)
        val access   = RacerDAPI.accessKindOfString(ae.elem.access.kind)
        val locks    = ae.elem.locks.map(l => RacerDAPI.lockOfString(l)).filter(l => {
          if (l == "") println("REMOVING EMPTY LOCK")
          l.resource != ""
        })
        val line = ae.loc
        val trace = RacerDAPI.traceOfListOfStrings(ae.trace)
        val hash = ae.hash
        /* TODO fix id below replace cls and id */
        (new RFSumm(file, cls, proc, resource, access, locks, line, trace, hash)) :: acc2
      })
  }
}
