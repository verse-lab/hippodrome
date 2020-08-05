package  org.racerdfix.language

/*  bug */
class TraceElem(val level: Int, val filename: String, val line_number: Int,val column_number: Int, val description: String)

/* summary */
class Access(val kind: String, val exp: String)
class Elem(val access: Access,val thread: String,val locks: List[String],val ownership_pre: String)
class AccessElem(val elem: Elem, val loc: Int, val trace: List[String], val hash: String)

/* RacerD's data structures*/
sealed trait RacerDDS
  case class Bug(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
                 val proc: String, val proc_start: Int, val file: String, bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String,
                 val access: String,
                 val snapshot1: String, val snapshot2: String)
  case class Summary(val file: String, val procedure: String, val accesses: List[AccessElem])
