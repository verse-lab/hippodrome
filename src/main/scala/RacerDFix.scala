package org.racerdfix

import org.racerdfix.language.{BugIn, BugOut, FBug, PatchBlock, RFSumm, SummaryIn}
import org.racerdfix.TraverseJavaClass.mainAlgo
import org.racerdfix.inferAPI.{InterpretJson, TranslationResult}
import org.racerdfix.utils.{ASTManipulation, BugsMisc, BugsStore, Logging, PatchStore}
import spray.json.DeserializationException

import scala.collection.mutable.HashMap
import scala.io.StdIn.readLine
import scala.sys.process._


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

    opt[Boolean]('n', "intellij").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(intellij = b))
    }.text("runs RacerDFix in IntelliJ mode - runs infer only once")

    opt[Boolean]('l', "log").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(log = b))
    }.text("logs all the applied patches")

    opt[String]('b', "json_bugs").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_bugs = b))
    }.text("sets the file which provides the bug details. The default one is " + Globals.json_bugs_file)

    opt[String]('s', "json_summaries").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_summaries = b))
    }.text("sets the file which provides the methods' summaries. The default one is " + Globals.json_summaries)

    opt[String]('r', "json_patches").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_patches = b))
    }.text("sets the file where the generated patches will be stored. The default one is " + Globals.json_patches)

    opt[String]('g', "log_file").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(json_patches = b))
    }.text("sets the logging. The default one is " + Globals.log_file)

    opt[String]('j', "java_sources_path").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(java_sources_path = b))
    }.text("the path to the source files. The default one is " + Globals.def_src_path)

    opt[String]('p', "infer").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer = b))
    }.text("the infer process to be called for generating json and validating. The default one is " + Globals.def_infer)

    opt[Seq[String]]('o', "infer_opt").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer_opt = b))
    }.text("the options infer runs with. The default one is " + Globals.def_infer_options)

    opt[Seq[String]]('f', "infer_target_files").action { (b, rc) =>
      rc.copy(fixConfig = rc.fixConfig.copy(infer_target_files = b))
    }.text("the target files to analyse and fix. The default one is " + Globals.def_target_files)

    opt[String]('c', "config_file").action { (b, rc) =>
      rc.copy(fixConfig = {
        val jsonTranslator = new InterpretJson(rc.fixConfig.copy(config_file = b))
        val infer_config = jsonTranslator.getJsonConfig()
        rc.fixConfig.copy(config_file = b, infer = infer_config.infer, infer_opt = infer_config.infer_opt, infer_target_files = infer_config.infer_target_files, json_path = infer_config.json_path)})
    }.text("the config file to setup infers. The default one is " + Globals.config_file)


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
        Logging.init(fixConfig)
        runPatchAndFix(fixConfig,true)
        Logging.stop
      case None =>
        System.err.println("Bad argument format.")
    }
  }

  /* Generating uniwue references */
  var patchID_ref = 1

  def patchIDGenerator(): Int = {
    val patchID = patchID_ref
    patchID_ref = patchID+ 1
    patchID
  }

  def patchIDGeneratorRange(len: Int): (Int, Int) = {
    val patchID_start = patchID_ref
    val patchID_stop  = patchID_ref + len + 1
    patchID_ref = patchID_stop + 1
    (patchID_start, patchID_stop)
  }

  def runPatchAndFix(config: FixConfig, initial_iteration: Boolean): Unit = {

    /* run infer */
    val infer   = config.infer
    val options = config.infer_opt
    val output_dir = Seq("--results-dir",config.json_path)
    val target_files   = config.infer_target_files
    val infer_process = Seq(infer).concat(options).concat(output_dir).concat(target_files)
    def fnc(a: Unit) : Unit  = {
       val res = infer_process.!
       if (res != 0) throw new Exception("Error while running infer: " + infer_process)
    }
    val res = Logging.addTime[(),Unit]("\n\n ################################## \n" +
                                                "Collect results from Infer took ", fnc, ())

    /* read the json files generated by infer and store the results into racerdfix's data structures */
    val jsonTranslator = new InterpretJson(config)
    val summariesIn = jsonTranslator.getJsonSummaries()
    val norm_and_translate = ((s:SummaryIn) => s.racerDToRacerDFix())
    val summaries   = summariesIn.results.flatMap(norm_and_translate)
    val bugsInAll   = jsonTranslator.getJsonBugs()
    val bugsIn      = new TranslationResult[BugIn](bugsInAll.results.filter(b => b.isInstanceOf[BugIn] &&
      Globals.tackled_bug_kinds.contains(b.asInstanceOf[BugIn].bug_type)).map(b => b.asInstanceOf[BugIn]))
    val bugs        = bugsIn.results.map(b => b.racerDToRacerDFix(summaries))

    /* for each bug in `bugs` find a patch and possibly generate a fix */
    val ast = new ASTManipulation
    val patchStore = new PatchStore

    val bugsStore = new BugsStore
    bugs.foreach(bug => bugsStore.update(bug))

    bugsStore.map.foreach( fbugs => {
      val bugs_str = fbugs._2.foldLeft("")((acc,bug) => acc + ", "  + bug.hash)
      println("**************** BUGS: " + bugs_str + " ************* ")
      patchStore.bug = fbugs._2.head.hash
      Logging.add("*********************************")
      Logging.add("bugs: " + bugs_str)

      val bugs_group = fbugs._2
      val summs = bugs_group.foldLeft[List[RFSumm]](Nil)((acc,bug) =>  {acc ++ BugsMisc.retrieveSummary(bug)})
      mainAlgo(summs, config, ast, patchStore)
    })
//      println("**************** BUG: " + bug.hash + " ************* ")
//      patchStore.bug = bug.hash
//      Logging.add("*********************************")
//      Logging.add("bug: " + bug.hash)
//      bug.snapshot2 match {
//        case Nil => {/* possibly Unprotected write */
//          /* assumes that snapshot1 is non-empty*/
//          println("Unprotected Write")
//          val summ1 = bug.snapshot1.foldLeft(bug.snapshot1.head)((acc,summ) => {
//            if (cost(summ.getCost(costSumm), acc.getCost(costSumm ))) summ
//            else acc
//          } )
//          mainAlgo(summ1, None, config, ast, patchStore)
//        }
//        case _   => {/* Read/Write */
//          /* assumes that snapshot1 is non-empty*/
//          println("Read/Write Race")
//          val summ1 = bug.snapshot1.foldLeft(bug.snapshot1.head)((acc,summ) => {
//            if (cost(summ.getCost(costSumm), acc.getCost(costSumm ))) summ
//            else acc
//          } )
//          val summ2 = bug.snapshot2.foldLeft(bug.snapshot2.head)((acc,summ) => {
//            if (cost(summ.getCost(costSumm), acc.getCost(costSumm ))) summ
//            else acc
//          } )
//          mainAlgo(summ1, Some(summ2), config, ast, patchStore)
//        }
//      }
//    })
//

//      bugs.foreach(bug => {
//      println("**************** BUG: " + bug.hash + " ************* ")
//      patchStore.bug = bug.hash
//      Logging.add("*********************************")
//      Logging.add("bug: " + bug.hash)
//
//        /* assumes that snapshot1 is non-empty*/
//        val summ1 = bug.snapshot1.foldLeft[Option[RFSumm]](None)((acc,summ) => {
//          acc match {
//            case None => Some(summ)
//            case Some(summ2) => if (lessExpensive(summ.getCost(costSumm), summ2.getCost(costSumm ))) Some(summ) else  acc
//          }
//        } )
//        val summ2 = bug.snapshot2.foldLeft[Option[RFSumm]](None)((acc,summ) => {
//          acc match {
//            case None => Some(summ)
//            case Some(summ2) => if (lessExpensive(summ.getCost(costSumm), summ2.getCost(costSumm ))) Some(summ) else  acc
//          }
//        } )
//        val summ1_lst = summ1 match {
//          case Some(summ) => List(summ)
//          case None => Nil
//        }
//        val summ2_lst = summ2 match {
//          case Some(summ) => List(summ)
//          case None => Nil
//        }
//        mainAlgo((summ1_lst ++ summ2_lst), config, ast, patchStore)
//    })

    /* Write the fixes to files */
    /* TODO for logging purposes, but also for generating the json, perhaps ast should also account for the patches*/
    ast.dumpAll(config, initial_iteration)
    val bugsOut = bugsIn.results.map(bug => bug.toBugOut(patchStore))
    jsonTranslator.printJsonBugsResults(bugsOut)

    /* VALIDATON */
    if(!(config.intellij)) {
      //println("INFER COMMAND:" + infer_process)
      val val_res = infer_process.!
      if (val_res != 0) throw new Exception("Error while running infer: " + infer_process)
      /* read the json files generated by infer and store the results into racerdfix's data structures*/
      /* TODO : for testing recheck the fix.java file instead of the original file */
      val newBugsInAll = jsonTranslator.getJsonBugs()
      val diffBugsNo = bugsInAll.results.length - bugsIn.results.length
      if (diffBugsNo < newBugsInAll.results.length) {
        println("New bugs detected during validation phase. Rerun RacerDFix? (Y/n)")
        val patch_id_str = readLine()
        if (patch_id_str == "Y") runPatchAndFix(config, false)
        else if (patch_id_str != "n") println("Unrecognized answer.")
      }
    }
  }

  def main(args: Array[String]) = handleInput(args)
}