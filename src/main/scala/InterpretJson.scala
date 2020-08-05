package org.racerdfix

import org.racerdfix.language.{Access, AccessElem, Bug, Elem, Summary, TraceElem}
import spray.json._

case class BugsResult[T](val results: List[T])
case class TraceResult[T](val trace: List[T])
case class SummaryResult[T](val accesses: List[T])


object BugProtocol extends DefaultJsonProtocol {

  def traceElemToJson(trace: TraceElem) ={
    JsObject(
      "level"         -> JsNumber(trace.level),
      "filename"      -> JsString(trace.filename),
      "line_number"   -> JsNumber(trace.line_number),
      "column_number" -> JsNumber(trace.column_number),
      "description"   -> JsString(trace.description)
    )
  }

  def jsonToTraceElem(value: JsValue) = {
    value.asJsObject.getFields(  "level",
      "filename",
      "line_number",
      "column_number",
      "description") match {
      case Seq( JsNumber(level),
      JsString(filename),
      JsNumber(line_number),
      JsNumber(column_number),
      JsString(description)) =>
        new TraceElem(level.toInt,filename,line_number.toInt,column_number.toInt,description)
      case _ => throw new DeserializationException("BugShort expected")
    }
  }

  def bugToJson(bug: Bug) = {
    JsObject(
      "bug_type" -> JsString(bug.bug_type),
      "qualifier" -> JsString(bug.qualifier),
      "severity"   -> JsString(bug.severity),
      "line" -> JsNumber(bug.line),
      "column" -> JsNumber(bug.column),
      "procedure" -> JsString(bug.proc),
      "procedure_start_line" -> JsNumber(bug.proc_start),
      "file" -> JsString(bug.file),
      "bug_trace"-> JsArray(bug.bug_trace.map(e => traceElemToJson(e)).toVector),
      "key"  -> JsString(bug.key),
      "hash" -> JsString(bug.hash),
      "bug_type_hum" -> JsString(bug.bug_type_hum),
      "access" -> JsString(bug.access),
      "snapshot1"  -> JsString(bug.snapshot1),
      "snapshot2"  -> JsString(bug.snapshot2)
    )
  }

  def jsonToBug(value: JsValue) = {
    value.asJsObject.getFields(      "bug_type",
      "qualifier",
      "severity",
      "line",
      "column",
      "procedure",
      "procedure_start_line",
      "file",
      "bug_trace",
      "key",
      "hash",
      "bug_type_hum",
      "access",
      "snapshot1",
      "snapshot2") match {
      case Seq(
      JsString(bug_type),
      JsString(qualifier),
      JsString(severity),
      JsNumber(line),
      JsNumber(column),
      JsString(proc),
      JsNumber(proc_start),
      JsString(file),
      JsArray(vector),
      JsString(access),
      JsString(key),
      JsString(hash),
      JsString(bug_type_hum),
      JsString(snapshot1),
      JsString(snapshot2)) =>
        val trace = vector.map(e => jsonToTraceElem(e)).toList
        new Bug(bug_type, qualifier,severity,line.toInt,column.toInt,proc,proc_start.toInt,file,trace,key,hash,bug_type_hum, access,snapshot1,snapshot2)
      case _ => throw new DeserializationException("BugShort expected")
    }
  }

  implicit object BugJsonFormat extends RootJsonFormat[BugsResult[Bug]] {
    def write(c: BugsResult[Bug]) : JsValue  = JsArray(
      c.results.map(c => bugToJson(c)).toVector
    )

    def read(value: JsValue) = {
      println("VALUE: " + value)
      value match {
        case JsArray(vector) => new BugsResult[Bug](vector.map(value => jsonToBug(value)).toList)
        case _ => throw new DeserializationException("Array of BugShort expected")
      }
    }
  }
}

object SummaryProtocol extends DefaultJsonProtocol {

  def accessToJson(ac: Access) ={
    JsObject(
      "kind"  -> JsString(ac.kind),
      "exp"   -> JsString(ac.exp)
    )
  }

  def elemToJson(elem: Elem) = {
    JsObject(
      "access"  -> accessToJson(elem.access),
      "thread"  -> JsString(elem.thread),
      "locks"   -> JsArray(elem.locks.map(e => JsString(e)).toVector),
      "ownership_pre" -> JsString(elem.ownership_pre)
    )
  }

  def accessElemToJson(ae: AccessElem) = {
    JsObject(
      "elem"    -> elemToJson(ae.elem),
      "loc"     -> JsNumber(ae.loc),
      "trace"   -> JsArray(ae.trace.map(e => JsString(e)).toVector),
      "hash"    -> JsString(ae.hash)
    )
  }

  /* case class Summary(val file: String, val procedure: String, val accesses: List[AccessElem]) */
  def summaryToJson(sum: Summary) = {
    JsObject(
      "file"       -> JsString(sum.file),
      "procedure"  -> JsString(sum.procedure),
      "accesses"   -> JsArray(sum.accesses.map(ae => accessElemToJson(ae)).toVector)
    )
  }


  def jsonToAccess(value: JsValue) ={
    value.asJsObject.getFields(
      "kind",
      "exp"
    ) match {
      case  Seq(JsString(kind),
        JsString(exp)) => new Access(kind,exp)
      case _ => throw new DeserializationException("Access expected")
    }
  }

  def jsonToString(e: JsValue) = {
    e match {
      case JsString(str) => str
      case _ => throw new DeserializationException("String expected")
    }
  }

  def jsonToElem(value: JsValue) ={
    value.asJsObject.getFields(
      "access",
      "thread",
      "locks",
      "ownership_pre"
    ) match {
      case  Seq(
      access,
      JsString(thread),
      JsArray(locks),
      JsString(ownership_pre))
          => new Elem(jsonToAccess(access),thread,locks.map(e=>jsonToString(e)).toList,ownership_pre)
      case _ => throw new DeserializationException("Elem expected")
    }
  }

  def jsonToAccessElem(value: JsValue) ={
    value.asJsObject.getFields(
      "elem",
      "loc",
      "trace",
      "hash"
    ) match {
      case  Seq(
      elem,
      JsNumber(loc),
      JsArray(trace),
      JsString(hash))
      => new AccessElem(jsonToElem(elem),loc.toInt,trace.map(e=>jsonToString(e)).toList,hash)
      case _ => throw new DeserializationException("AccessElem expected")
    }
  }

  def jsonToSummary(value: JsValue) ={
    value.asJsObject.getFields(
      "file",
      "procedure",
      "accesses"
    ) match {
      case  Seq(
      JsString(file),
      JsString(procedure),
      JsArray(accesses))
      => new Summary(file,procedure,accesses.map(e=>jsonToAccessElem(e)).toList)
      case _ => throw new DeserializationException("Summary expected")
    }
  }

  implicit object SummaryJsonFormat extends RootJsonFormat[SummaryResult[Summary]] {
    def write(c: SummaryResult[Summary]) : JsValue  = JsArray(
      c.accesses.map(c => summaryToJson(c)).toVector
    )

    def read(value: JsValue) = {
      value match {
        case JsArray(vector) => new SummaryResult[Summary](vector.map(value => jsonToSummary(value)).toList)
        case _ => throw new DeserializationException("Array of BugShort expected")
      }
    }
  }
}


class InterpretJson {

  val source  = """{ "some": "JSON source" }"""
  val jsonAst = source.parseJson
  val json    = jsonAst.prettyPrint

  def testJsonBugs(config: FixConfig) = {
    import BugProtocol._
    val fm  = new FileManipulation
    val src = fm.fileToString(config.json_bugs)
    val jsonAst = src.parseJson
    val json    = jsonAst.prettyPrint
    val bugsDS  = jsonAst.convertTo[BugsResult[Bug]]
    json
  }

  def testJsonSummary(config: FixConfig) = {
    import SummaryProtocol._
    val fm  = new FileManipulation
    val src = fm.fileToString(config.json_summaries)
    val jsonAst = src.parseJson
    val json    = jsonAst.prettyPrint
    val bugsDS  = jsonAst.convertTo[SummaryResult[Summary]]
    json
  }
}
