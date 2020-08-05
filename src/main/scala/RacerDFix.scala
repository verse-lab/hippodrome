package org.racerdfix

import language.{CSumm, Read, Write}
import org.racerdfix.TraverseJavaClass.mainAlgo
import org.racerdfix.language.CSumm


object RacerDFix {

  case class RunConfig(fixConfig: FixConfig, fileName: String)

  private val defaultPath = "src/test/java/"
  private val TOOLNAME = "RacerDFix"
  private val SCRIPTNAME = "racerdfix"
  private val VERSION = "0.1"
  private val VERSION_STRING = s"v$VERSION"

  private val parser = new {

  } with scopt.OptionParser[RunConfig](SCRIPTNAME) {
    // See examples at https://github.com/scopt/scopt

    head(TOOLNAME, VERSION_STRING)

    opt[String]("fileName").action {(x, c) =>
      c.copy(fileName = x)
    }.text("a synthesis file name (the file under the specified folder, called filename.syn)")

    opt[Boolean]('i', "interactive").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(interactive = b))
    }.text("runs RacerDFix in interactive mode - the user is expected to choose a patch")

    opt[Boolean]('t', "testing").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(testing = b))
    }.text("runs RacerDFix in testing mode - generated fixes do not overwrite the original file")

    opt[String]('b', "json_bugs").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_bugs = b))
    }.text("sets the file which provides the bug details. The default one is " + Globals.json_bugs_file)

    opt[String]('b', "json_summaries").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_bugs = b))
    }.text("sets the file which provides the methods' summaries. The default one is " + Globals.json_summaries)

    opt[String]('b', "json_patches").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_bugs = b))
    }.text("sets the file where the generated patches will be stored. The default one is " + Globals.json_patches)

    help("help").text("prints this usage text")

  }

  def parseParams(paramString: Array[String], params: FixConfig): FixConfig = {
    val newConfig = RunConfig(params, defaultPath)
    parser.parse(paramString, newConfig) match {
      case Some(RunConfig(fixConfig, _)) => fixConfig
      case None => throw RacerDFixException("Bad argument format.")
    }
  }

  private def handleInput(args: Array[String]): Unit = {
    val newConfig = RunConfig(FixConfig(), defaultPath)
    parser.parse(args, newConfig) match {
      case Some(RunConfig(fixConfig, file)) =>
        /* TODO read the JSONS and store them in data structures */
//        val ij = new InterpretJson()
//        println("json bugs: " + ij.testJsonBugs(newConfig.fixConfig))
//        println("json summaries: " + ij.testJsonSummary(newConfig.fixConfig))
        runPatchAndFix(fixConfig)
      case None =>
        System.err.println("Bad argument format.")
    }
  }


  def runPatchAndFix(config: FixConfig) = {
//    val bugs =
    val filename = "src/test/java/RacyFalseNeg.java"
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val csumm1 = new CSumm(filename,"B","this->myA->f", Read, List("P<0>{(this:B*).myA2}"), 30 )
    val csumm2 = new CSumm(filename,"B","this->myA", Write, List("P<0>{(this:B*).myA1}"), 24 )
    //
    //    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30 )
    //    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24 )
    mainAlgo(csumm1, csumm2, config)

  }

  def main(args: Array[String]) = handleInput(args)
}
