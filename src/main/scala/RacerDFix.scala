package org.racerdfix

import org.racerdfix.language.{BugIn, Bugs, RFSumm, SummaryIn}
import org.racerdfix.ProcessOneBugGroup.mainAlgo
import org.racerdfix.inferAPI.{InterpretJson, TranslationResult}
import org.racerdfix.utils.{ASTManipulation, BugsMisc, BugsStore, FileManipulation, Logging, PatchStore}

import scala.io.StdIn.readLine
import scala.sys.process._


object RacerDFix {

  private val parser = ArgParser.argsParser

  def parseParams(paramString: Array[String], params: FixConfig): FixConfig = {
    val newConfig = RunConfig(params, Globals.def_src_path)
    parser.parse(paramString, newConfig) match {
      case Some(RunConfig(fixConfig, _)) => fixConfig
      case None => throw RacerDFixException("Bad argument format.")
    }
  }

  private def handleInput(args: Array[String]): Unit = {
    args.foreach( str => println("args: " + str))
    val newConfig = RunConfig(FixConfig(), Globals.def_src_path)
    parser.parse(args, newConfig) match {
      case Some(RunConfig(fixConfig, file)) =>
        Logging.init(fixConfig)
        runPatchAndFix(fixConfig, 1)
        Logging.stop
      case None =>
        System.err.println("Bad argument format.")
    }
  }

  /* Generating unique references */
  var patchID_ref = 1

  def patchIDGenerator(): String = {
    val patchID = patchID_ref
    patchID_ref = patchID+ 1
    patchID.toString
  }

  def patchIDGeneratorRange(len: Int): (Int, Int) = {
    val patchID_start = patchID_ref
    val patchID_stop  = patchID_ref + len + 1
    patchID_ref = patchID_stop + 1
    (patchID_start, patchID_stop)
  }

  def shouldBePatched(config: FixConfig, bug: BugIn) = {
    config.prio_files.length == 0 ||
    (config.prio_files.length > 0 && config.prio_files.contains(bug.asInstanceOf[BugIn].file))
  }

  def runPatchAndFix(config: FixConfig, iteration: Int): Int = {

    /* run infer */
    if(config.flag1) println("RacerDFix started!")
    val infer   = config.infer
    val options = config.infer_opt
    val output_dir = Seq("--results-dir",config.json_path)
    val target_files  = config.infer_target_files
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
    val summaries0  = summariesIn.results.flatMap(norm_and_translate)
    val summaries   = Globals.distinct_eq( (a:RFSumm,b:RFSumm) => a.equals(b), summaries0)
    val bugsInAll   = jsonTranslator.getJsonBugs()
    /* filter out bugs not in prio_files */
    val bugsAllPrio = new TranslationResult[BugIn](bugsInAll.results.filter( b =>
      b.isInstanceOf[BugIn] && shouldBePatched(config,b.asInstanceOf[BugIn])).map(b => b.asInstanceOf[BugIn]))
    /* filter out all the bugs not related to data races */
    val bugsIn      = new TranslationResult[BugIn](bugsAllPrio.results.filter(b => b.isInstanceOf[BugIn] &&
      Globals.tackled_bug_kinds.contains(b.asInstanceOf[BugIn].bug_type)).map(b => b.asInstanceOf[BugIn]))
    val bugs        = bugsIn.results.map(b => b.racerDToRacerDFix(summaries))
    if(config.flag1) println("Reading json files completed!")

    /* for each bug in `bugs` find a patch and possibly generate a fix */
    val ast = new ASTManipulation
    val patchStore = new PatchStore

    val bugsStore = new BugsStore
    bugs.foreach(bug => bugsStore.update(bug))
    if(config.flag1) println("Bug Store updated!")

    bugsStore.map.foreach( fbugs => {
      val bugs_str = fbugs._2._2.foldLeft("")((acc,bug) => acc + ", "  + bug.hash)
      println("**************** BUGS: " + fbugs._1 + " =>" + bugs_str + " ************* ")
      patchStore.bug = fbugs._2._2.head.hash
      Logging.add("*********************************")
      Logging.add("bugs: " + bugs_str)

      val bugs_group = fbugs._2._2
      val summs = bugs_group.foldLeft[List[RFSumm]](Nil)((acc,bug) =>  {acc ++ BugsMisc.retrieveSummary(bug)})
      mainAlgo(summs, config, ast, patchStore)
    })

    /* Write the fixes to files */
    /* TODO for logging purposes, but also for generating the json, perhaps ast should also account for the patches*/
    ast.dumpAll(config, iteration == 1)
    val bugsOut = bugsIn.results.map(bug => bug.toBugOut(patchStore))
    jsonTranslator.printJsonBugsResults(bugsOut)

    /* VALIDATON */
    val ret =
    if(!(config.intellij)) {
      val val_res = infer_process.!
      if (val_res != 0) throw new Exception("Error while running infer: " + infer_process)
      /* read the json files generated by infer and store the results into racerdfix's data structures*/
      /* TODO : for testing recheck the fix.java file instead of the original file */
      val newBugsInAll = jsonTranslator.getJsonBugs()
      val diffBugsNo = bugsInAll.results.length - bugsIn.results.length
      if (diffBugsNo < newBugsInAll.results.length) {
        println("New bugs detected during validation phase. Rerun RacerDFix? (Y/n)")
        val answer_str = if (config.interactive) { readLine() } else if (iteration < config.iterations) "Y" else "n"
        if (answer_str == "Y") runPatchAndFix(config, iteration +1 )
        else
          if (answer_str == "n") 0
          else  {
           println("Unrecognized answer.")
           0;
         }
      } else  1
    } else  1
   if(config.testing) ast.resetToOrig(config)
   return ret;
  }

  def main(args: Array[String]) = handleInput(args)
}