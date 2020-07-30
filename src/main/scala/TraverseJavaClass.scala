package com.racerdfix

import com.racerdfix.antlr.{Java8Lexer, Java8Parser}
import com.racerdfix.fixdsl.{CSumm, Insert, Read, Update, Write}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token, TokenStreamRewriter}

import scala.io.Source
import scala.io.StdIn.readLine

class PatchResult(val patch: String, var rewriter: TokenStreamRewriter,var start: Token, var stop: Token) {

  override def toString() : String = {
    patch
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

  def parseContent(content: String) ={
    val java8Lexer = new Java8Lexer(CharStreams.fromString(content))
    val tokens = new CommonTokenStream(java8Lexer)
    val java8Parser = new Java8Parser(tokens)
    val tree = java8Parser.compilationUnit
    (tokens,tree)
  }

  /* ************************************************ */
  /*                       UPDATE                     */
  /* ************************************************ */
  def generateUpdatePatch(filename: String, update: Update, id: Int,
                          rewriter: TokenStreamRewriter,
                          tree: Java8Parser.CompilationUnitContext): Option[PatchResult] ={
    val syncVisitor   = new SynchronizedVisitor
    val rewriter_temp = new TokenStreamRewriter(rewriter.getTokenStream)
    syncVisitor.setFix(update)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getSynchronizedBlock
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.updateSynchronizedStatement(rewriter_temp,sblock,update)
        println("Patch ID: " + id)
        println("Replace lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine))
        println("-: " + oldcode)
        println("+: " + patch)
        Some(new PatchResult(patch, rewriter, sblock.start, sblock.stop))
      case None =>
        println("No patch could be generated for attempt ID: " + id)
        None
    }
  }

  /* ************************************************ */
  /*                       INSERT                     */
  /* ************************************************ */
  def generateInsertPatch_def(filename: String, insert: Insert, id: Int,
                              rewriter: TokenStreamRewriter,
                              tree: Java8Parser.CompilationUnitContext): Option[PatchResult] = {
    val syncVisitor   = new SynchronizedVisitor
    val rewriter_temp = new TokenStreamRewriter(rewriter.getTokenStream)
    syncVisitor.setFix(insert)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getResourceStatement
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter_temp,sblock,insert)
        println("Patch ID: " + id)
        println("Replace lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine))
        println("+: " + patch)
        Some(new PatchResult(patch, rewriter, sblock.start, sblock.stop))
      case None =>
        println("No patch could be generated for attempt ID: " + id)
        None
    }
  }

  /* debugger */
  def generateInsertPatch(filename: String, insert: Insert, id: Int,
                          rewriter: TokenStreamRewriter,
                          tree: Java8Parser.CompilationUnitContext): Option[PatchResult] = {
    val res = generateInsertPatch_def(filename,insert,id,rewriter,tree)
    if(false) {
      println(insert)
      println("id:  " + id)
      println("out: " + Globals.getTextOpt[PatchResult](res))
    }
    res
  }

  /* ************************************************ */
  /*                         FIX                      */
  /* ************************************************ */
  def applyPatch_def(filename: String,
                 patch_id: Int,
                 patches: List[(Int,Option[PatchResult])]): Unit = {
    val patch = patches.foldLeft(None: Option[PatchResult])((acc: Option[PatchResult],
                                                             ptc: (Int, Option[PatchResult])) => {
      if (ptc._1 == patch_id) ptc._2
      else acc
    })
    patch match {
      case None => println("Invalid patch ID")
      case Some(patch: PatchResult) =>
        val rewriter = patch.rewriter
        rewriter.replace(patch.start, patch.stop, patch.patch)

        /* write to file (keep the original one in `filename.orig` and the fix in `filename` */
        val fm = new FileManipulation
        fm.cloneOriginalFile(filename)
        fm.overwriteOriginalFile(filename, rewriter.getText)
    }
  }

  def applyPatch_helper(filename: String,
                     patch_id_str: String,
                     patches: List[(Int,Option[PatchResult])]): Unit = {
    try {
      val patch_id = patch_id_str.toInt
      applyPatch_def(filename,patch_id,patches)
    } catch {
      case _: Exception => {println("Invalid patch ID")
        None
      }
    }
  }

  /* debugger */
  def applyPatch(filename: String,
                 patch_id_str: String,
                 patches: List[(Int,Option[PatchResult])]): Unit = {
    if (false){
      println("Patch id: " + patch_id_str)
      println("Patches: "  + patches)
    }
    applyPatch_helper(filename,patch_id_str,patches)
  }

  /* Generates a list of UPDATE objects meant to update the locks in
  * `form_summ` with locks in `to_summ` */
  def generateUpdateObjects(from_summ: CSumm, to_summ: CSumm): List[Update] = {
    from_summ.lock.foldLeft(List[Update]())((acc, lock) => {
      val lck  = RacerDAPI.getLock2Var(lock)
      val locks = to_summ.lock.foldLeft(acc)((acc2, lock2) => Update("B",from_summ.line,lck,RacerDAPI.getLock2Var(lock2))::acc2)
      acc ++ locks
    }
    )
  }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
     based on the locks available in summary `locks_summ` */
  def generateInsertObjects(locks_summ: CSumm, resource_summ: CSumm): List[Insert] = {
    locks_summ.lock.foldLeft(List[Insert]())((acc, lock) => {
      val lck    = RacerDAPI.getLock2Var(lock)
      val insert = Insert("B",resource_summ.line,RacerDAPI.getResource2Var(resource_summ.resource),lck)
      insert::acc }
    )
  }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
     based on the locks available in summary `locks_summ` */
  def generateInsertObjectOnCommonResource(summ1: CSumm, summ2: CSumm): List[Insert] = {
    /* TODO  need to check which is that common resouce, e.g. myA or myA.f?
    *   should be the outer most one, e.g. myA*/
    val lck    = RacerDAPI.getResource2Var(summ1.resource)
    val insert1 = Insert("B",summ1.line,RacerDAPI.getResource2Var(summ1.resource),lck)
    val insert2 = Insert("B",summ2.line,RacerDAPI.getResource2Var(summ2.resource),lck)
    List(insert1,insert2)
  }

  /* Generates a list of patches corresponding to the list of UPDATE objects (updates) */
  def generateUpdatePatches(filename: String,
                            updates: List[Update],
                            rewriter: TokenStreamRewriter,
                            tree: Java8Parser.CompilationUnitContext): List[(Int, Option[PatchResult])] = {
      if(updates.length > 0) {
        val patchIDInterval = patchIDGenerator(updates.length)
        val updates_id = updates.zip(List.range(patchIDInterval._1, patchIDInterval._2))
        updates_id.foldLeft(List[(Int,Option[PatchResult])]())((acc,update) => {
          val res = generateUpdatePatch(filename, update._1,update._2,rewriter,tree)
          (update._2,res)::acc
        })
      } else List.empty
  }

  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateInsertPatches(filename: String,
                            inserts: List[Insert],
                            rewriter: TokenStreamRewriter,
                            tree: Java8Parser.CompilationUnitContext): List[(Int, Option[PatchResult])] = {
    if(inserts.length > 0) {
      val patchIDInterval = patchIDGenerator(inserts.length)
      val inserts_id = inserts.zip(List.range(patchIDInterval._1,patchIDInterval._2))
      inserts_id.foldLeft(List[(Int,Option[PatchResult])]())((acc,insert) => {
        val res = generateInsertPatch(filename, insert._1,insert._2,rewriter,tree)
        (insert._2,res)::acc
      })
    } else List.empty
  }

  def mainAlgo(filename: String): Unit = {
    /* ******** */
    /*   PARSE  */
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val (tokens,tree) = parseContent(javaClassContent)
    val rewriter = new TokenStreamRewriter(tokens)

    println("************* GENERATE PATCH *************")
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
//    val csumm1 = new CSumm("this->myA->f", Read, List("P<0>{(this:B*).myA2}"), 30 )
//    val csumm2 = new CSumm("this->myA", Write, List("P<0>{(this:B*).myA1}"), 24 )

    val csumm1 = new CSumm("this->myA->f", Read, List(), 30 )
    val csumm2 = new CSumm("this->myA", Write, List(), 24 )

    if ( true /*csumm1.resource == csumm2.resource*/) {
      val commun_locks = csumm1.lock intersect csumm2.lock

      /* Both statements are synchronized but there is no common lock ==>
        *  1. UPDATE one of the locks with a lock from the peer statement
        *  2. INSERT a new synchronization over one of the two statements  */

      val patches1 = if (csumm1.lock.length > 0 && csumm2.lock.length > 0 && commun_locks.length ==0) {
        /* ************** UPDATES ***************** */
        /* generate update objects */
        /* TODO need to check which locks suit to be changed (e.g. they don't protect other resources) */
        /* currently just generate blindly all possibilities */
        val updates1 = generateUpdateObjects(csumm1,csumm2)
        val updates2 = generateUpdateObjects(csumm2,csumm1)
        val updates    = updates1 ++ updates2

        /* generate update patches */
        val update_patches = generateUpdatePatches(filename, updates, rewriter, tree)

        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts1 = generateInsertObjects(csumm1,csumm2)
        val inserts2 = generateInsertObjects(csumm2,csumm1)
        val inserts = inserts1 ++ inserts2

        /* generate inserts patches */
        val insert_patches = generateInsertPatches(filename, inserts, rewriter, tree)

        update_patches ++ insert_patches
      } else List.empty

      /* The resource in `csumm1` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `csumm2` to protect the
       * statement manipulating the resource in `csumm1`  */

      val patches2 = if (csumm1.lock.length == 0 && csumm2.lock.length > 0) {
        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts = generateInsertObjects(csumm2,csumm1)

        /* generate insert patches */
        generateInsertPatches(filename,inserts,rewriter,tree)
      } else List.empty

      /* The resource in `csumm2` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `csumm` to protect the
       * statement manipulating the resource in `csumm2`  */

      val patches3 = if (csumm1.lock.length > 0 && csumm2.lock.length == 0) {
        /* ************** INSERTS ***************** */
        /* generate insert objects */
        val inserts = generateInsertObjects(csumm1,csumm2)

        /* generate insert patches */
        generateInsertPatches(filename,inserts,rewriter,tree)
      }  else List.empty

      val patches = patches1 ++ patches2 ++ patches3

      if(patches.length >0 ) {

        println("Choose a patch <enter patch id>")
        val patch_id_str = readLine()

        println("************* GENERATE FIX *************")
        applyPatch(filename, patch_id_str, patches)
      } else {

        /* Both of the resources `csumm1` and `csumm2` are not synchronized ==>
       * INSERT a new synchronization using a lock on the common resource  */

        val patches4 = if (csumm1.lock.length == 0 && csumm2.lock.length == 0) {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          val inserts = generateInsertObjectOnCommonResource(csumm1, csumm2)

          /* generate insert patches */
          generateInsertPatches(filename, inserts, rewriter, tree)
        } else List.empty


        println("************* GENERATE FIX *************")
        patches4.foreach(elem=> applyPatch_def(filename, elem._1, List(elem)))
      }
    }
  }

  def main(args: Array[String]) = {
    //applyUpdate("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    //applyInsert("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    mainAlgo("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java")

//    val racerdapi = new RacerDAPI()
//    val insert = Insert("B", 24, racerdapi.getResource2Var("this->myA"), racerdapi.getLock2Var("P<0>{(this:B*).myA4444}"))
//    val res =  generateInsertPatch("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java", insert,1)
  }
}
