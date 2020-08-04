package org.racerdfix

import spray.json._

class InterpretJson {

  val source  = ""
  val jsonAst = source.parseJson
  val json    = jsonAst.prettyPrint

  def testJson() = {
    val fm  = new FileManipulation
    val src = fm.fileToString("src/test/JSON/racerdfix_summaries.json")
    val jsonAst = src.parseJson
    val json    = jsonAst.prettyPrint
    json
  }
}
