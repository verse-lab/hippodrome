package  org.racerdfix.language

import org.racerdfix.inferAPI.RacerDAPI

/*  bug */
class TraceElem(val level: Int, val filename: String, val line_number: Int,val column_number: Int, val description: String)

/* summary */
class Access(val kind: String, val exp: String)
class Elem(val access: Access,val thread: String,val locks: List[String],val ownership_pre: String)
class AccessElem(val elem: Elem, val loc: Int, val trace: List[String], val hash: String)

/* RacerD's data structures*/

class Bug(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
          val proc: String, val proc_start: Int, val file: String,
          val bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String, val access: String,
          val snapshot1: String, val snapshot2: Option[String]){

}

class Summary(val file: String, val procedure: String, var accesses: List[AccessElem]) {
  def updateAccesses(accesses: List[AccessElem]) = {
    this.accesses = accesses
  }
  
  def racerDToRacerDFix(hash: String): List[CSumm] = {
    this.accesses.foldLeft(List.empty[CSumm])((acc2, ae) => {
      if (ae.hash == hash) {
        val cls = RacerDAPI.classNameOfMethodString(procedure)
        val resource = RacerDAPI.getResource2Var(ae.elem.access.exp)
        val access = RacerDAPI.accessKindOfString(ae.elem.access.kind)
        val locks = ae.elem.locks.map(l => RacerDAPI.lockOfString(l))
        val line = ae.loc
        val trace = RacerDAPI.traceOfListOfStrings(ae.trace)
        (new CSumm(file, cls, resource, access, locks, line, trace)) :: acc2
      }
      else acc2
    })

  }
}
