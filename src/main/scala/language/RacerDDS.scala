package  org.racerdfix.language

import org.racerdfix.inferAPI.RacerDAPI

/*  bug */
class TraceElem(val level: Int, val filename: String, val line_number: Int,val column_number: Int, val description: String)

/* summary */
class Access(val kind: String, val exp: String)
class Elem(val access: Access,val thread: String,val locks: List[String],val ownership_pre: String)
class AccessElem(val elem: Elem, val loc: Int, val trace: List[String], val hash: String)

/* RacerD's data structures*/

class BugIn(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
            val proc: String, val proc_start: Int, val file: String,
            val bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String, val access: String,
            val snapshot1_hash: String, val snapshot2_hash: Option[String]){

  def racerDToRacerDFix(summaries: List[RFSumm]): FBug = {
    val summ1 = summaries.filter(sum => sum.hash == snapshot1_hash)
    val summ2 = snapshot2_hash match {
      case None => Nil
      case Some(str)  => summaries.filter(sum => sum.hash == str)
    }
    new FBug(summ1, summ2, hash)
  }
}

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
        val cls = RacerDAPI.classNameOfMethodString(procedure)
        val resource = RacerDAPI.varOfResource(ae.elem.access.exp)
        val access = RacerDAPI.accessKindOfString(ae.elem.access.kind)
        val locks = ae.elem.locks.map(l => RacerDAPI.lockOfString(l))
        val line = ae.loc
        val trace = RacerDAPI.traceOfListOfStrings(ae.trace)
        val hash = ae.hash
        (new RFSumm(file, cls, resource, access, locks, line, trace, hash)) :: acc2
      })
  }
}
