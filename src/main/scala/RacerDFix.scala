package org.racerdfix

import language.{AccessElem, RFSumm, EmptyTrace, Lock, Read, SummaryIn, Write}
import org.racerdfix.TraverseJavaClass.mainAlgo
import org.racerdfix.inferAPI.{InterpretJson, RacerDAPI}


object RacerDFix {

  case class RunConfig(fixConfig: FixConfig, fileName: String)

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

    opt[String]('s', "json_summaries").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_summaries = b))
    }.text("sets the file which provides the methods' summaries. The default one is " + Globals.json_summaries)

    opt[String]('p', "json_patches").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_patches = b))
    }.text("sets the file where the generated patches will be stored. The default one is " + Globals.json_patches)

    opt[String]('j', "java_sources_path").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(java_sources_path = b))
    }.text("teh path to teh source files. The default one is " + Globals.def_src_path)


    help("help").text("prints this usage text")

  }

  def parseParams(paramString: Array[String], params: FixConfig): FixConfig = {
    val newConfig = RunConfig(params, Globals.def_src_path)
    parser.parse(paramString, newConfig) match {
      case Some(RunConfig(fixConfig, _)) => fixConfig
      case None => throw RacerDFixException("Bad argument format.")
    }
  }

  private def handleInput(args: Array[String]): Unit = {
    val newConfig = RunConfig(FixConfig(), Globals.def_src_path)
    parser.parse(args, newConfig) match {
      case Some(RunConfig(fixConfig, file)) =>
        runPatchAndFix(fixConfig)
      case None =>
        System.err.println("Bad argument format.")
    }
  }

  def costSumm(summ: RFSumm) = {
    summ.trace.length()
  }

  def cost( val1: Int, val2: Int) = val1 <= val2

  def runPatchAndFix(config: FixConfig) = {
    val jsonTranslator = new InterpretJson(config)
    val summariesIn = jsonTranslator.getJsonSummaries()
    val norm_and_translate = ((s:SummaryIn) => s.updatePathReturn(config.java_sources_path)).andThen((s:SummaryIn) => s.racerDToRacerDFix())
    val summaries   = summariesIn.results.flatMap(norm_and_translate)
    val bugsIn      = jsonTranslator.getJsonBugs()
    val bugs        = bugsIn.results.map(b => b.racerDToRacerDFix(summaries))
    /* for each bug in `bugs` find a patch and possibly generate a fix */
    bugs.foreach(bug => {
      bug.snapshot2 match {
        case Nil => /* possibly Unprotected write */
        case _   => {
          /* assumes that snapshot1 is non-empty*/
          val summ1 = bug.snapshot1.foldLeft(bug.snapshot1.head)((acc,summ) => {
            if (cost(summ.getCost(costSumm), acc.getCost(costSumm ))) summ
            else acc
          } )
          val summ2 = bug.snapshot2.foldLeft(bug.snapshot2.head)((acc,summ) => {
            if (cost(summ.getCost(costSumm), acc.getCost(costSumm ))) summ
            else acc
          } )
          mainAlgo(summ1, summ2, config)
        }
      }
    })


    val filename = "src/test/java/RacyFalseNeg.java"
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
    val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
    val csumm1 = new RFSumm(filename,"B","this->myA->f", Read, List(lock1), 30, EmptyTrace, "")
    val csumm2 = new RFSumm(filename,"B","this->myA", Write, List(lock2), 24, EmptyTrace, "")
    //
    //    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30, EmptyTrace, "")
    //    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24, EmptyTrace, "")
    mainAlgo(csumm1, csumm2, config)

  }

  def main(args: Array[String]) = handleInput(args)
}