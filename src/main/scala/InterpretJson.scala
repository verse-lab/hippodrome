package org.racerdfix

import org.racerdfix.language.{Bug, BugShort, TraceElem}
import spray.json._

case class BugsResult[T](val results: List[T])
case class TraceResult[T](val trace: List[T])


object BugShortProtocol extends DefaultJsonProtocol {

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

  def bugToJson(bug: BugShort) = {
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
        new BugShort(bug_type, qualifier,severity,line.toInt,column.toInt,proc,proc_start.toInt,file,trace,key,hash,bug_type_hum, access,snapshot1,snapshot2)
      case _ => throw new DeserializationException("BugShort expected")
    }
  }

  implicit object BugShortJsonFormat extends RootJsonFormat[BugsResult[BugShort]] {
    def write(c: BugsResult[BugShort]) : JsValue  = JsArray(
      c.results.map(c => bugToJson(c)).toVector
    )

    def read(value: JsValue) = {
      println("VALUE: " + value)
      value match {
        case JsArray(vector) => new BugsResult[BugShort](vector.map( value => jsonToBug(value)).toList)
        case _ => throw new DeserializationException("Array of BugShort expected")
      }
    }
  }
}

class InterpretJson {

  val source  = """{ "some": "JSON source" }"""
  val jsonAst = source.parseJson
  val json    = jsonAst.prettyPrint

  def testJson(config: FixConfig) = {
//    val fm  = new FileManipulation
//    val src = fm.fileToString(config.json_bugs)
//    val jsonAst = src.parseJson
//    val json    = jsonAst.prettyPrint
    import BugShortProtocol._
    val fm  = new FileManipulation
    val src = fm.fileToString(config.json_bugs)
    val jsonAst = src.parseJson
    val json    = jsonAst.prettyPrint
    val bugsDS  = jsonAst.convertTo[BugsResult[BugShort]]
    println("JSON OBJECT: " + bugsDS.toString())
    json
  }
}
