package org.racerdfix

import language.{AccessElem, CSumm, EmptyTrace, Lock, Read, Summary, Write}
import org.racerdfix.TraverseJavaClass.mainAlgo
import org.racerdfix.inferAPI.{InterpretJson, RacerDAPI}


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
    val jsonTranslator = new InterpretJson(config)
    val bugs = jsonTranslator.getJsonBugs()
    val summaries = jsonTranslator.getJsonSummaries()
    /* for each bug in `bugs` find a patch and possibly generate a fix */
    bugs.results.foreach(bug => {
      val snapshot1_hash = bug.snapshot1
      val snapshot2_hash = bug.snapshot2
      /* there could be multiple snapshots with the same hash if they refer to the same problematic resource operation. */
//      val snapshot1_lst = summaries.snapshots.foldLeft(List.empty[Summary])((acc, summ) => {
//          val x = summ.accesses.foldLeft(List.empty[AccessElem])((acc2,ae) => {
//          if (ae.hash == snapshot1_hash) ae::acc2
//          else acc2
//        })
//        if (x.length > 0) summ.updateAccesses(x) ::acc
//        else acc
//      })
//      snapshot2_hash match {
//        case None => {
//            /* unprotected write */
//            /* generate an insert for the inner most statement */
//            val snapshot1 = snapshot1_lst.foldLeft(None)((acc,summ) =>{
//              acc match {
//                case None => acc
//                case Some(snp) => if(snp.)
//              }
//            })
//        }
//        case Some(snapshot2_hash) =>
//      }

    }
    )
    val filename = "src/test/java/RacyFalseNeg.java"
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
    val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
    val csumm1 = new CSumm(filename,"B","this->myA->f", Read, List(lock1), 30, EmptyTrace)
    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(lock2), 24, EmptyTrace )
    //
    //    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30 )
    //    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24 )
    mainAlgo(csumm1, csumm2, config)

  }

  def main(args: Array[String]) = handleInput(args)
}