package  org.racerdfix.language

class TraceElem(val level: Int, val filename: String, val line_number: Int,val column_number: Int, val description: String)

/* RacerD's data structures*/
sealed trait RacerDDS
 case class Bug(val access: String, val bug_trace: String /*Array[TraceElem]*/ , val bug_type: String, val bug_type_hum: String, val column: Int,
                val file: String, val hash: String, val key: String, val line: Int, val proc: String, val proc_start: Int,
                val qualifier: String, val severity: String, val snapshot1: String, val snapshot2: String)
 case class BugShort(val bug_type: String, val qualifier: String, val severity: String, val line: Int, val column: Int,
                     val proc: String, val proc_start: Int, val file: String, bug_trace: List[TraceElem], val key: String, val hash: String, val bug_type_hum: String,
                     val access: String,
                     val snapshot1: String, val snapshot2: String)
