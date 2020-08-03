package com.racerdfix

import com.racerdfix.antlr.{Java8Lexer, Java8Parser}
import com.racerdfix.fixdsl.{CSumm, FileModif, Insert, PatchBlock, Read, Summ, Update, Write}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token, TokenStream, TokenStreamRewriter}

import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap


class GroupByRewriterPatchResult(var map : HashMap[FileModif, List[PatchBlock]]) {

  def update(fm: FileModif, pb: PatchBlock): Unit = {
    try {
      this.map(fm) = pb :: this.map(fm)
    } catch {
      case e => {
        this.map(fm) = List(pb)
      }
    }
  }

  def getText(): String = {
    map.foldLeft("")((acc, x) => acc + (
        (x._2.foldLeft("")((acc, y) => acc + "\n" + y.description))))
  }
}

/* maps patch ID -> list of individual code modifications */
class GroupByIdPatchResult(var map : HashMap[Int, GroupByRewriterPatchResult]) {

  def update(id: Int, fm: FileModif, pb: PatchBlock): Unit = {
    try {
      this.map(id).update(fm,pb)
    } catch {
      case e => {
        this.map(id) = new GroupByRewriterPatchResult(HashMap(fm -> List(pb)))
      }
    }
  }

  def getText(): String = {
    map.foldLeft("")((acc, x) => acc + (
      "\n" +
        "==========================" +
        "\n" +
        "Patch ID: " + x._1 + x._2.getText()))
  }

}


object TraverseJavaClass  {

  var patchID_ref = 1

  def patchIDGenerator(len: Int): (Int, Int) = {
    val patchID_start = patchID_ref
    val patchID_stop  = patchID_ref + len + 1
    patchID_ref = patchID_stop + 1
    (patchID_start, patchID_stop)
  }

  def parseContent(filename: String) = {
    val javaClassContent = Source.fromFile(filename).getLines.foldLeft("") { (str, line) => str + " \n " + line.toString }
    val java8Lexer = new Java8Lexer(CharStreams.fromString(javaClassContent))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    val rewriter = new TokenStreamRewriter(tokens)
    (tokens, tree, rewriter)
  }

  /* ************************************************ */
  /*                       UPDATE                     */
  /* ************************************************ */

  /* Generates a list of UPDATE objects meant to update the locks in
  * `form_summ` with locks in `to_summ` */
  def generateUpdateObjects(from_summ: CSumm, to_summ: CSumm): List[Update] = {
    from_summ.lock.foldLeft(List[Update]())((acc, lock) => {
      val lck  = RacerDAPI.getLock2Var(lock)
      val locks = to_summ.lock.foldLeft(acc)((acc2, lock2) => Update(from_summ.cls,from_summ.line,lck,RacerDAPI.getLock2Var(lock2))::acc2)
      acc ++ locks
    }
    )
  }

  def generateUpdatePatch(update: Update,
                          tokens: TokenStream,
                          tree: Java8Parser.CompilationUnitContext): Option[PatchBlock] ={
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(tokens)
    syncVisitor.setFix(update)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getSynchronizedBlock
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.updateSynchronizedStatement(rewriter,sblock,update)
//        println("Patch ID: " + id)
        val description = (
        "Replace (UPDATE) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
        + ("\n" + "-: " + oldcode)
        + ("\n" + "+: " + patch) )
        Some(new PatchBlock(patch, sblock.start, sblock.stop, description))
      case None =>
        println("No patch could be generated for attempt ID: " )
        None
    }
  }

  /* ************************************************ */
  /*                       INSERT                     */
  /* ************************************************ */

  /* Generates a list of INSERT objects for the resource in `resource_summ`
   based on the locks available in summary `locks_summ` */
  def generateInsertObjects(locks_summ: CSumm, resource_summ: CSumm): List[Insert] = {
    locks_summ.lock.foldLeft(List[Insert]())((acc, lock) => {
      val lck    = RacerDAPI.getLock2Var(lock)
      val insert = Insert(resource_summ.cls,resource_summ.line,RacerDAPI.getResource2Var(resource_summ.resource),lck)
      insert::acc }
    )
  }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
     based on the locks available in summary `locks_summ` */
  def generateInsertObjectOnCommonResource(summ1: CSumm, summ2: CSumm): (List[Insert], List[Insert]) = {
    /* TODO  need to check which is that common resouce, e.g. myA or myA.f?
    *   should be the outer most one, e.g. myA*/
    val lck    = RacerDAPI.getResource2Var(summ1.resource)
    val insert1 = Insert(summ1.cls,summ1.line,RacerDAPI.getResource2Var(summ1.resource),lck)
    val insert2 = Insert(summ2.cls,summ2.line,RacerDAPI.getResource2Var(summ2.resource),lck)
    (List(insert1),List(insert2))
  }

  def generateInsertPatch_def(insert: Insert,
                              tokens: TokenStream,
                              tree: Java8Parser.CompilationUnitContext): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(tokens)
    syncVisitor.setFix(insert)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getResourceStatement
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert)
        val description = (
        "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
          + ("\n" + "-: " + oldcode)
          + ("\n" + "+: " + patch) )
        Some(new PatchBlock(patch, sblock.start, sblock.stop, description))
      case None =>
        println("No patch could be generated for attempt ID " )
        None
    }
  }

  /* debugger */
  def generateInsertPatch(insert: Insert,
                          tokens: TokenStream,
                          tree: Java8Parser.CompilationUnitContext): Option[PatchBlock] = {
    val res = generateInsertPatch_def(insert, tokens, tree)
    if(false) {
      println(insert)
      println("out: " + Globals.getTextOpt[PatchBlock](res))
    }
    res
  }

  /* ************************************************ */
  /*                         FIX                      */
  /* ************************************************ */

  def applyPatch_def(
                 patch_id: Int,
                 patches: GroupByIdPatchResult): Unit = {
    try {
      val patches0 = patches.map(patch_id)
      patches0.map.foreach( x => {
        val rewriter = x._1.rewriter
        val filename = x._1.filename
        val patches  = x._2
        patches.foreach( pb => rewriter.replace(pb.start, pb.stop, pb.patch))

        /* write to file (keep the original one in `filename.orig` and the fix in `filename` */
        val fm = new FileManipulation
        fm.cloneOriginalFile(filename)
        fm.overwriteOriginalFile(filename, rewriter.getText)
      })
    } catch {
      case _ => println("Invalid patch ID")
    }
  }

  def applyPatch_helper(
                     patch_id_str: String,
                     patches: GroupByIdPatchResult): Unit = {
    try {
      val patch_id = patch_id_str.toInt
      applyPatch_def(patch_id,patches)
    } catch {
      case _: Exception => {println("Invalid patch ID")
        None
      }
    }
  }

  /* debugger */
  def applyPatch(
                 patch_id_str: String,
                 patches: GroupByIdPatchResult): Unit = {
    if (false){
      println("Patch id: " + patch_id_str)
      println("Patches: "  + patches)
    }
    applyPatch_helper(patch_id_str,patches)
  }

  /* Generates a list of patches corresponding to the list of UPDATE objects (updates) */
  def generateUpdatePatches(filename: String,
                            updates: List[Update],
                            tokens: TokenStream,
                            tree: Java8Parser.CompilationUnitContext,
                            id: Option[Int] = None): List[(Int, Option[PatchBlock])] = {
    if(updates.length > 0) {
      val ids =
      id match {
        case None => {
          val patchIDInterval = patchIDGenerator(updates.length)
          List.range(patchIDInterval._1, patchIDInterval._2)
        }
        case Some(id) => List.fill(updates.length)(id)
      }
      val updates_id = updates.zip(ids)
      updates_id.foldLeft(List[(Int,Option[PatchBlock])]())((acc, update) => {
        val res = generateUpdatePatch(update._1,tokens,tree)
        (update._2,res)::acc
      })
    } else List.empty
  }

  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateInsertPatches(filename: String,
                            inserts: List[Insert],
                            tokens: TokenStream,
                            tree: Java8Parser.CompilationUnitContext,
                            id: Option[Int] = None): List[(Int, Option[PatchBlock])] = {
    if(inserts.length > 0) {
      val ids =
        id match {
          case None => {
            val patchIDInterval = patchIDGenerator(inserts.length)
            List.range(patchIDInterval._1, patchIDInterval._2)
          }
          case Some(id) => List.fill(inserts.length)(id)
        }
      val inserts_id = inserts.zip(ids)
      inserts_id.foldLeft(List[(Int,Option[PatchBlock])]())((acc, insert) => {
        val res = generateInsertPatch(insert._1,tokens,tree)
        (insert._2,res)::acc
      })
    } else List.empty
  }

  def generateGroupPatches(groupByIdPatchResult: GroupByIdPatchResult,
                           patches: List[(Int, Option[PatchBlock])], summ: Summ) = {
//    var empty_map = new GroupByIdPatchResult(HashMap.empty[Int, GroupByRewriterPatchResult])
    patches.foldLeft(groupByIdPatchResult)((acc: GroupByIdPatchResult, ptch) =>
      ptch._2 match {
        case None => acc
        case Some(pb) => {
          acc.update(ptch._1, summ.fm, pb)
          acc
        }
      }
    )
  }

  def mainAlgo(csumm1: CSumm, csumm2: CSumm): Unit = {
    /* ******** */
    /*   PARSE  */
    val (summ1,summ2) = if (csumm1.filename == csumm2.filename) {
      val javaFile = csumm1.filename
      val (tokens, tree, rewriter) = parseContent(javaFile)
      val fm = new FileModif(javaFile,rewriter)
      val summ1 = new Summ(fm, tree, tokens, csumm1)
      val summ2 = new Summ(fm, tree, tokens, csumm2)
      (summ1,summ2)
    } else {
      val (tokens1, tree1, rewriter1) = parseContent(csumm1.filename)
      val (tokens2, tree2, rewriter2) = parseContent(csumm2.filename)
      val fm1 = new FileModif(csumm1.filename,rewriter1)
      val fm2 = new FileModif(csumm2.filename,rewriter2)
      val summ1 = new Summ(fm1, tree1, tokens1, csumm1)
      val summ2 = new Summ(fm2, tree2, tokens2, csumm2)
      (summ1,summ2)
    }

    println("************* GENERATE PATCH *************")
//    if ( true /*csumm1.resource == csumm2.resource*/) {
    var empty_map = new GroupByIdPatchResult(HashMap.empty[Int, GroupByRewriterPatchResult])
    if ((csumm1.lock concat csumm2.lock).length > 0) {
      val common_locks = csumm1.lock intersect csumm2.lock

      /* Both statements are synchronized but there is no common lock ==>
        *  1. UPDATE one of the locks with a lock from the peer statement
        *  2. INSERT a new synchronization over one of the two statements  */

      val patches1 = if (csumm1.lock.length > 0 && csumm2.lock.length > 0 && common_locks.length ==0) {
        /* ************** UPDATES ***************** */
        /* generate update objects */
        /* TODO need to check which locks suit to be changed (e.g. they don't protect other resources) */
        /* currently just generate blindly all possibilities */
        val updates1 = generateUpdateObjects(summ1.csumm,summ2.csumm)
        val updates2 = generateUpdateObjects(summ2.csumm,summ1.csumm)

        /* generate update patches */
        val update_patches1 = generateUpdatePatches(summ1.csumm.filename, updates1, summ1.tokens, summ1.tree)
        val update_patches2 = generateUpdatePatches(summ2.csumm.filename, updates2, summ2.tokens, summ2.tree)

        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts1 = generateInsertObjects(summ1.csumm,summ2.csumm)
        val inserts2 = generateInsertObjects(summ2.csumm,summ1.csumm)

        /* generate inserts patches */
        val insert_patches1 = generateInsertPatches(summ1.csumm.filename, inserts1, summ1.tokens, summ1.tree)
        val insert_patches2 = generateInsertPatches(summ2.csumm.filename, inserts2, summ2.tokens, summ2.tree)

        val patches1 = generateGroupPatches(empty_map,update_patches1 ++ insert_patches1,summ1)
        val patches2 = generateGroupPatches(patches1, update_patches2 ++ insert_patches2,summ2)
        patches2
      } else empty_map

      /* The resource in `summ1` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `summ2` to protect the
       * statement manipulating the resource in `csumm1`  */

      val patches2 = if (summ1.csumm.lock.length == 0 && summ2.csumm.lock.length > 0) {
        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts = generateInsertObjects(summ2.csumm,summ1.csumm)

        /* generate insert patches */
        val patches = generateInsertPatches(summ1.csumm.filename, inserts, summ1.tokens, summ1.tree)
        generateGroupPatches(patches1, patches,summ1)
      } else patches1

      /* The resource in `csumm2` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `csumm` to protect the
       * statement manipulating the resource in `csumm2`  */

      val patches3 = if (csumm1.lock.length > 0 && csumm2.lock.length == 0) {
        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts = generateInsertObjects(summ1.csumm,summ2.csumm)

        /* generate insert patches */
        val patches = generateInsertPatches(summ2.csumm.filename, inserts, summ2.tokens, summ2.tree)
        generateGroupPatches(patches2, patches,summ2)
      }  else patches2

      val grouped_patches = patches3
      println(grouped_patches.getText())

      println("Choose a patch <enter patch id>")
      val patch_id_str = readLine()

      println("************* GENERATE FIX *************")
      applyPatch(patch_id_str, grouped_patches)

    } else {

      /* Both of the resources `csumm1` and `csumm2` are not synchronized ==>
       * INSERT new synchronization using a lock on the common resource  */

      val patch_id  = patchIDGenerator(0)._2
      var empty_map = new GroupByIdPatchResult(HashMap.empty[Int, GroupByRewriterPatchResult])
      val grouped_patches = if (csumm1.lock.length == 0 && csumm2.lock.length == 0) {
        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val (inserts1, inserts2) = generateInsertObjectOnCommonResource(csumm1, csumm2)

        /* generate insert patches */
        val patches1 = generateInsertPatches(summ1.csumm.filename, inserts1, summ1.tokens, summ1.tree, Some(patch_id))
        val patches2 = generateInsertPatches(summ2.csumm.filename, inserts2, summ2.tokens, summ2.tree, Some(patch_id))
        val grouped_patches1 = generateGroupPatches(empty_map, patches1, summ1)
        val grouped_patches2 = generateGroupPatches(grouped_patches1, patches2, summ2)
        grouped_patches2
      } else empty_map


      println(grouped_patches.getText())

      println("************* GENERATE FIX *************")
      applyPatch_def(patch_id,grouped_patches)
    }
  }

  def main(args: Array[String]) = {
    val filename = "/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java"
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val csumm1 = new CSumm(filename,"B","this->myA->f", Read, List("P<0>{(this:B*).myA2}"), 30 )
    val csumm2 = new CSumm(filename,"B","this->myA", Write, List("P<0>{(this:B*).myA1}"), 24 )

//    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30 )
//    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24 )
    mainAlgo(csumm1, csumm2)

//    val racerdapi = new RacerDAPI()
//    val insert = Insert("B", 24, racerdapi.getResource2Var("this->myA"), racerdapi.getLock2Var("P<0>{(this:B*).myA4444}"))
//    val res =  generateInsertPatch("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java", insert,1)
  }
}

/**
 * TODO implement the cost function and choose a patch accordingly
 * TODO work on the JSON interface with Infer
 * TODO check how to avoid insertion of new lines with rewriter
 * TODO create a set of variables for all possible combinations: {this.A, A} ... what about the fields access?
 * */