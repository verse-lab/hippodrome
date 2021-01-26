package org.racerdfix

import org.racerdfix.language.{BugIn, Bugs, FBug, RFSumm, SummaryIn}
import org.racerdfix.ProcessOneBugGroup.{mainAlgo, applyPatch}
import org.racerdfix.inferAPI.{InterpretJson, TranslationResult}
import org.racerdfix.utils.{ASTManipulation, BugsMisc, BugsStore, FileManipulation, Logging, PatchStore}

import scala.io.StdIn.readLine
import scala.sys.process._


object Hippodrome {

  private val parser = ArgParser.argsParser

  def parseParams(paramString: Array[String], params: FixConfig): FixConfig = {
    val newConfig = RunConfig(params, Globals.def_src_path)
    parser.parse(paramString, newConfig) match {
      case Some(RunConfig(fixConfig, _)) => fixConfig
      case None => throw RacerDFixException("Bad argument format.")
    }
  }

  private def handleInput(args: Array[String]): Int = {
    args.foreach( str => println("args: " + str))
    val newConfig = RunConfig(FixConfig(), Globals.def_src_path)
    parser.parse(args, newConfig) match {
      case Some(RunConfig(fixConfig, file)) =>
        val config_from_json = fixConfig.config_options.toArray
        config_from_json.foreach( str => println("args: " + str))
        val fixConfig_ext = {
          parser.parse(config_from_json, RunConfig(fixConfig, fixConfig.java_sources_path)) match {
            case Some(RunConfig(fixConfig, file)) => fixConfig
            case None => fixConfig
          }
        }
        Logging.init(fixConfig_ext)
        val res = runPatchAndFix(fixConfig_ext, 1)
        Logging.stop
        res
      case None =>
        System.err.println("Bad argument format.")
        1
    }
  }

  /* Generating unique references */
  var patchID_ref = 1
  var labelID_ref = 1

  def patchIDGenerator(): String = {
    val patchID = patchID_ref
    patchID_ref = patchID+ 1
    patchID.toString
  }

  def labelIDGenerator(): String = {
    val labelID = labelID_ref
    labelID_ref = labelID+ 1
    Globals.label_def + labelID.toString
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

  def applyPatchWithValidaton(choiceId: String, groups: GroupByIdPatchOptions, patchStore: PatchStore,
                              config: FixConfig,
                              ast: ASTManipulation,
                              diffBugsNo: Int,
                              jsonTranslator: InterpretJson,
                              infer: Unit => Int): Unit = {
    applyPatch(choiceId, groups, patchStore)
    /* Validate */
    ast.dumpAllTemp(config)
    infer(())
    ast.resetFromTempToOrig(config)
    val newBugsInAll = jsonTranslator.getJsonBugs()
    val bugsIn = new TranslationResult[BugIn](newBugsInAll.results.filter(b => b.isInstanceOf[BugIn] &&
      Globals.tackled_bug_kinds.contains(b.asInstanceOf[BugIn].bug_type)).map(b => b.asInstanceOf[BugIn]))
    val newDiffBugsNo = newBugsInAll.results.length - bugsIn.results.length
    if (newDiffBugsNo > diffBugsNo) {
      /* revert  + remove the patch from patchstore too*/
      /* remove the chosen patchId from patchstore */
      /* update the choice id to the next patch in line */
      /* apply the next in line patch */
      println("The current patch introduces new bugs. Revert back and perhaps try another patch. ")
      ast.rollbackLastPatch
      patchStore.removePatch(choiceId)
      val newChoiceId = patchStore.getFirstInLine()
      newChoiceId match {
        case None =>
        case Some(id) => applyPatchWithValidaton(id, patchStore.getPatches(), patchStore, config, ast, diffBugsNo, jsonTranslator, infer)
      }
    }
  }

  def patchOneCluster(config: FixConfig, bugs: (String, (scala.List[String], scala.List[FBug])),
                      patchStore: PatchStore,
                      ast: ASTManipulation,
                      diffBugsNo: Int,
                      jsonTranslator: InterpretJson,
                      infer: Unit => Int): Unit = {
    /* set a CHECKPOINT to return to in case of invalid patch */
    //val ast_clone = ast.map
    val bugs_str = bugs._2._2.foldLeft("")((acc,bug) => acc + ", "  + bug.hash)
    println("**************** BUGS: " + bugs._1 + " =>" + bugs_str + " ************* ")
    patchStore.bug = bugs._2._2.head.hash
    Logging.add("*********************************")
    Logging.add("bugs: " + bugs_str)

    val bugs_group = bugs._2._2
    val summs = bugs_group.foldLeft[List[RFSumm]](Nil)((acc,bug) =>  {acc ++ BugsMisc.retrieveSummary(config,bug)})
    val summs_distinct = Globals.distinct_eq((a:RFSumm,b:RFSumm) => a.equals(b),summs)
    ast.initPatch()
    val res = mainAlgo(summs_distinct, config, ast, patchStore)
    res match {
      case None =>
      case Some(res) => {
        val choiceId = res._1
        applyPatchWithValidaton(choiceId,res._2,patchStore, config, ast, diffBugsNo, jsonTranslator, infer)
      }
    }
  }

  def runPatchAndFix(config: FixConfig, iteration: Int): Int = {

    /* run infer */
    if(config.flag1) println(Globals.TOOLNAME + " started!")
    val infer   = config.infer
    val options = config.infer_opt
    val output_dir = Seq("--results-dir",config.json_path)
    val target_files  = config.infer_target_files
    val infer_process = Seq(infer).concat(options).concat(output_dir).concat(target_files)
    def fnc(a: Unit)  = {
       val res = infer_process.!
       if (res != 0) throw new Exception("Error while running infer: " + infer_process)
       res
    }
    if(iteration == 1) Logging.addTime[(),Unit]("\n\n ################################## \n" +
      "Collect results from Infer took ", fnc, ())

    /* read the json files generated by infer and store the results into racerdfix's data structures */
    val jsonTranslator = new InterpretJson(config)
    val summariesIn = jsonTranslator.getJsonSummaries()
    val norm_and_translate = ((s:SummaryIn) => s.racerDToRacerDFix())
    val summaries0  = summariesIn.results.flatMap(norm_and_translate)
    val summaries   = summaries0 //Globals.distinct_eq( (a:RFSumm,b:RFSumm) => a.equals(b), summaries0)
    val bugsInAll   = jsonTranslator.getJsonBugs()
    /* filter out bugs not in prio_files */
    val bugsAllPrio = new TranslationResult[BugIn](bugsInAll.results.filter( b =>
      b.isInstanceOf[BugIn] && shouldBePatched(config,b.asInstanceOf[BugIn])).map(b => b.asInstanceOf[BugIn]))
    /* filter out all the bugs not related to data races */
    val bugsIn      = new TranslationResult[BugIn](bugsAllPrio.results.filter(b => b.isInstanceOf[BugIn] &&
      Globals.tackled_bug_kinds.contains(b.asInstanceOf[BugIn].bug_type)).map(b => b.asInstanceOf[BugIn]))
    val bugs        = bugsIn.results.map(b => b.racerDToRacerDFix(summaries))

    /* for each bug in `bugs` find a patch and possibly generate a fix */
    val ast = new ASTManipulation
    val patchStore = new PatchStore

    val bugsStore = new BugsStore
    bugs.foreach(bug => bugsStore.update(bug))
    //ast.saveOriginal(config)

    bugsStore.map.foreach( fbugs => {
      patchOneCluster(config, fbugs, patchStore, ast, bugsInAll.results.length - bugsIn.results.length, jsonTranslator, fnc)
    })

    /* Write the fixes to files */
    /* TODO for logging purposes, but also for generating the json, perhaps ast should also account for the patches */
    ast.dumpAll(config, iteration == 1)
    val bugsOut = bugsIn.results.map(bug => bug.toBugOut(patchStore))
    jsonTranslator.printJsonBugsResults(bugsOut)

    /* VALIDATON */
    val ret =
      try {
        if (!(config.intellij)) {
          Logging.addTime[(),Unit]("\n\n ################################## \n" +
            "Collect results from Infer took ", fnc, ())
          /* read the json files generated by infer and store the results into racerdfix's data structures*/
          /* TODO : for testing recheck the fix.java file instead of the original file */
          val newBugsInAll = jsonTranslator.getJsonBugs()
          val diffBugsNo = bugsInAll.results.length - bugsIn.results.length
          if (diffBugsNo < newBugsInAll.results.length) {
            println("New bugs detected during validation phase. Rerun " + Globals.TOOLNAME + "? (Y/n)")
            val answer_str = if (config.interactive) {
              readLine()
            } else if (iteration < config.iterations) "Y" else "n"
            if (answer_str == "Y") runPatchAndFix(config, iteration + 1)
            else if (answer_str == "n") 1
            else {
              println("Unrecognized answer.")
              1
            }
          } else 0
        } else 0
      }
    catch {
      case exc => throw exc
    } finally {
        if(config.testing) ast.resetToOrig(config)
        println(" End of " + Globals.TOOLNAME + " execution. ")
      }
   return ret;
  }

  def main(args: Array[String]): Unit = {
    val res = handleInput(args)
    System.exit(res)
  }

}