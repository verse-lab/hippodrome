package com.racerdfix

import com.racerdfix.antlr.{Java8Lexer, Java8Parser}
import com.racerdfix.fixdsl.{CSumm, Insert, Read, Update, Write}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token, TokenStreamRewriter}

import scala.io.Source
import scala.io.StdIn.readLine

class PatchResult(val patch: String, var rewriter: TokenStreamRewriter,var start: Token, var stop: Token)

object TraverseJavaClass  {

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
  def generateUpdatePatch(filename: String, update: Update, id: Int): Option[PatchResult] ={
    /* ******** */
    /*   PARSE  */
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val (tokens,tree) = parseContent(javaClassContent)
    val rewriter = new TokenStreamRewriter(tokens)

    /* ************** */
    /* GENERATE PATCH */
    val syncVisitor = new SynchronizedVisitor
    syncVisitor.setFix(update)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getSynchronizedBlock
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.updateSynchronizedStatement(rewriter,sblock,update)
        println("Patch ID: " + id)
        println("Replace lines: " + sblock.start.getLine + " - " + sblock.stop.getLine)
        println("-: " + oldcode)
        println("+: " + patch)
        Some(new PatchResult(patch, rewriter, sblock.start, sblock.stop))
      case None =>
        println("No patch could be generated for attempt ID: " + id)
        None
    }
  }

  def applyUpdatePatch( filename: String,
                        patch_id_str: String,
                       patches: List[(Int,Option[PatchResult])]): Unit = {
    try {
      val patch_id = patch_id_str.toInt
      val patch = patches.foldLeft(None:Option[PatchResult])((acc:Option[PatchResult],
                                                              ptc:(Int,Option[PatchResult])) => {
        if (ptc._1 == patch_id) ptc._2
        else acc
      })
      patch match {
        case None => println("Invalid patch ID")
        case Some(patch:PatchResult) =>
          val rewriter  = patch.rewriter
          rewriter.replace(patch.start,patch.stop,patch.patch)

          /* write to file (keep the original one in `filename.orig` and the fix in `filename` */
          val fm = new FileManipulation
          fm.cloneOriginalFile(filename)
          fm.overwriteOriginalFile(filename,rewriter.getText )
      }
    } catch {
      case e: Exception => None
    }
  }

  /* ************************************************ */
  /*                       INSERT                     */
  /* ************************************************ */
  def generateInsertPatch(filename: String, insert: Insert, id: Int): Option[PatchResult] = {
    /* ******** */
    /*   PARSE  */
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft(""){(str,line)=> str + " \n " + line.toString}
    val (tokens,tree) = parseContent(javaClassContent)
    val rewriter = new TokenStreamRewriter(tokens)

    /* ************** */
    /* GENERATE PATCH */
    val syncVisitor = new SynchronizedVisitor
    syncVisitor.setFix(insert)
    syncVisitor.visit(tree)
    val sblock = syncVisitor.getAssignmentBlock
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert)
        println("Patch ID: " + id)
        println("Replace lines: " + sblock.start.getLine + " - " + sblock.stop.getLine)
        println("+: " + patch)
        Some(new PatchResult(patch, rewriter, sblock.start, sblock.stop))
      case None =>
        println("No patch could be generated for attempt ID: " + id)
        None
    }
    None
  }

  def applyInsert(filename: String) {
    /* ******** */
    /*   PARSE  */
    val javaFile = filename
    val javaClassContent = Source.fromFile(javaFile).getLines.foldLeft("") { (str, line) => str + " \n " + line.toString }
    val (tokens, tree) = parseContent(javaClassContent)
    val rewriter = new TokenStreamRewriter(tokens)

    /* ************** */
    /* GENERATE PATCH */
    val racerdapi = new RacerDAPI()
    val insert = Insert("RacyFalseNeg.java", 24, racerdapi.getLock2Var("P<0>{(this:B*).myA4444}"))
    val syncVisitor = new SynchronizedVisitor
    syncVisitor.setFix(insert)
    syncVisitor.visit(tree)
    rewriter.insertBefore(syncVisitor.getAntlrLineNo(insert.line), "synchronized(myA4444){")
    rewriter.insertAfter(syncVisitor.getAntlrLineNo(insert.line), "}")

    println("==========================================")

    println("insert: " + rewriter.getText(tree.getSourceInterval))


  }

  def mainAlgo(filename: String): Unit = {

    println("************* GENERATE PATCH *************")
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val csumm1 = new CSumm("this->myA", Read, List("P<0>{(this:B*).myA2}"), 30 )
    val csumm2 = new CSumm("this->myA", Write, List("P<0>{(this:B*).myA1}"), 24 )
    if (csumm1.resource == csumm2.resource) {
      val commun_locks = csumm1.lock intersect csumm2.lock
      if (csumm1.lock.length > 0 && csumm2.lock.length > 0 && commun_locks.length ==0) {
        /* TODO need to check which locks suit to be changed (e.g. they don't protect other resources) */
        /* currently just generate blindly all possibilities */
        val racerdapi = new RacerDAPI()
        val updates1 = csumm1.lock.foldLeft(List[Update]())((acc, lock) => {
          val lck  = racerdapi.getLock2Var(lock)
          val locks = csumm2.lock.foldLeft(acc)((acc2, lock2) => Update("B",csumm1.line,lck,racerdapi.getLock2Var(lock2))::acc2)
          acc ++ locks
          }
        )
        val updates2 = csumm2.lock.foldLeft(List[Update]())((acc, lock) => {
          val lck  = racerdapi.getLock2Var(lock)
          val locks = csumm1.lock.foldLeft(acc)((acc2, lock2) => Update("B",csumm2.line,lck,racerdapi.getLock2Var(lock2))::acc2)
          acc ++ locks
        }
        )
        val updates    = updates1 ++ updates2
        if(updates.length > 0) {
          val updates_id = updates.zip(List.range(1,updates.length + 1))
          val patches = updates_id.foldLeft(List[(Int,Option[PatchResult])]())((acc,update) => {
            val res = generateUpdatePatch(filename, update._1,update._2)
            (update._2,res)::acc
          })
          println("Choose a patch <enter patch id>")
          val patch_id_str = readLine()
          println("************* GENERATE FIX *************")
          applyUpdatePatch(filename,patch_id_str,patches)
        }
      }
    }
    /* decide whether to update or insert */
  }

  def main(args: Array[String]) = {
    //applyUpdate("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    //applyInsert("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java");
    // mainAlgo("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java")

    val racerdapi = new RacerDAPI()
    val insert = Insert("RacyFalseNeg.java", 24, racerdapi.getLock2Var("P<0>{(this:B*).myA4444}"))
    val res =  generateInsertPatch("/Users/andrea/git/racerdfix/src/test/java/RacyFalseNeg.java", insert,1)
  }
}
