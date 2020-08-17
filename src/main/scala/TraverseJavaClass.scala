package org.racerdfix

import org.racerdfix.language.{And, FSumm, FixKind, InsAfter, InsBefore, InsertDeclareAndInst, InsertSync, NoFix, NoPatch, Or, PAnd, PInsert, POr, PUpdate, Patch, PatchBlock, PatchCost, RFSumm, Replace, UpdateSync}
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.antlr.Java8Parser
import org.antlr.v4.runtime.{TokenStream, TokenStreamRewriter}
import utils.{ASTManipulation, ASTStoreElem, FileModif, Logging, PatchStore}

import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap


//class GroupByRewriterPatch(var map : HashMap[FileModif, List[PatchBlock]]) {
//
//  def update(fm: FileModif, pb: PatchBlock): Unit = {
//    try {
//      this.map(fm) = pb :: this.map(fm)
//    } catch {
//      case e => {
//        this.map(fm) = List(pb)
//      }
//    }
//  }
//
//  def getText(): String = {
//    map.foldLeft("")((acc, x) => acc + (
//        (x._2.foldLeft("")((acc, y) => acc + "\n" + y.description))))
//  }
//
//  /* returns the cost of all the patches stored in this.map */
//  def getTotalCost(): PatchCost = {
//    this.map.foldLeft(Globals.unitCost)((acc,patches) => {
//      val cost = patches._2.foldLeft(Globals.unitCost)((acc,pb) => acc.add(pb.cost))
//      acc.add(cost)})
//  }
//}

/* maps patch ID -> list of individual code modifications */
class GroupByIdPatchOptions(var map : HashMap[Int, List[PatchBlock]]) {

  def update(id: Int, pb: PatchBlock): Unit = {
    try {
      this.map.update(id, this.map(id) ++ List(pb))
    } catch {
      case e => {
        this.map(id) = List(pb)
      }
    }
  }

  def getText(): String = {
    map.foldLeft("")((acc, x) => acc + (
      "\n" +
        "==========================" +
        "\n" +
        "Patch ID: " + x._1 + x._2.foldLeft("")((acc, y) => acc + "\n" + y.description)))
  }

  /* returns the cost of the patch with id `patchID` */
  def getCost(patchID: Int) = {
    this.map(patchID).foldLeft(Globals.unitCost)((acc,pb) => acc.add(pb.cost))
  }
}


object TraverseJavaClass  {

  /* ************************************************ */
  /*                       UPDATE                     */
  /* ************************************************ */

  /* Generates a list of UPDATE objects meant to update the locks in
  * `form_summ` with locks in `to_summ` */
  def generateUpdateObjects(from_summ: RFSumm, to_summ: RFSumm): List[UpdateSync] = {
    from_summ.locks.foldLeft(List[UpdateSync]())((acc, lock) => {
      val lck  = lock.resource
      val locks = to_summ.locks.foldLeft(acc)((acc2, lock2) => UpdateSync(from_summ.cls,from_summ.line,lck,lock2.resource)::acc2)
      acc ++ locks
    }
    )
  }

  def generateUpdatePatch(update: UpdateSync,
                          ast: ASTStoreElem): Option[PatchBlock] ={
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(update)
    syncVisitor.visit(ast.tree)
    val sblock = syncVisitor.getSynchronizedBlock
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.updateSynchronizedStatement(rewriter,sblock,update)
//        println("Patch ID: " + id)
        val description = (
        "Replace (UPDATE) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
        + ("\n" + "-: " + oldcode)
        + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost))
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
  def generateInsertObjects(locks_summ: RFSumm, resource_summ: RFSumm): List[InsertSync] = {
    locks_summ.locks.foldLeft(List[InsertSync]())((acc, lock) => {
      /* double check that there is no double-lock attempt */
      if (resource_summ.locks.exists(lck => lck.resource == lock.resource)) acc
      else {
        val lck = lock.resource
        val insert = InsertSync(resource_summ.cls, resource_summ.line, RacerDAPI.varOfResource(resource_summ.resource), lck)
        insert :: acc
      } } )
  }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
     based on the locks available in summary `locks_summ` */
  def generateInsertObjectOnCommonResource(summ1: RFSumm, summ2: RFSumm): (List[FixKind], List[FixKind]) = {
    /* TODO  need to check which is that common resouce, e.g. myA or myA.f?
    *   should be the outer most one, e.g. myA*/
    val lck    = RacerDAPI.varOfResource(summ1.resource)
    val varName = "obj" + RacerDFix.patchIDGenerator
    val declareObj = { if (summ1.line < summ2.line)  InsertDeclareAndInst(summ1.cls,summ1.line,"Object", varName)
    else InsertDeclareAndInst(summ2.cls,summ1.line,"Object", varName)}
    val insert1 = InsertSync(summ1.cls,summ1.line,RacerDAPI.varOfResource(summ1.resource),varName)
    val insert2 = InsertSync(summ2.cls,summ2.line,RacerDAPI.varOfResource(summ2.resource),varName)
    if (summ1.line < summ2.line)
      (List(And(declareObj,insert1)),List(insert2))
    else
      (List(insert1),List(And(declareObj,insert2)))
  }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
   based on the locks available in summary `locks_summ` */
  def generateInsertObjectOnUnprotectedResource(summ: RFSumm) = {
    /* TODO  need to check which is that common resouce, e.g. myA or myA.f?
    *   should be the outer most one, e.g. myA*/
    /* TODO need to recheck what is the resource we create lock for. myA.f is not the right type, it should be
    *   a reference type. */
    // val lck     = RacerDAPI.varOfResource(summ.resource)
    val varName = "obj" + RacerDFix.patchIDGenerator
    val declareObj = InsertDeclareAndInst(summ.cls,summ.line,"Object", varName)
    val insert1 = InsertSync(summ.cls,summ.line,RacerDAPI.varOfResource(summ.resource),varName)
    And(declareObj,insert1)
  }


  def generateInsertPatch_def(insert: InsertSync,
                              ast: ASTStoreElem): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(insert)
    syncVisitor.visit(ast.tree)
    val sblock = syncVisitor.getResourceStatement
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert)
        val description = (
        "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
          + ("\n" + "-: " + oldcode)
          + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost))
      case None =>
        println("No patch could be generated for attempt ID " )
        None
    }
  }

  /* debugger */
  def generateInsertPatch(insert: InsertSync,
                          ast: ASTStoreElem): Option[PatchBlock] = {
    val res = generateInsertPatch_def(insert, ast)
    if(false) {
      println(insert)
      println("out: " + Globals.getTextOpt[PatchBlock](res))
    }
    res
  }


  def generateInsertDeclareAndInstPatch_def(insert: InsertDeclareAndInst,
                              ast: ASTStoreElem): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(insert)
    syncVisitor.visit(ast.tree)
    val sblock = syncVisitor.getClassStatement
    (sblock,syncVisitor.getClassStart) match {
      case (Some(sblock),Some(start)) =>
        val (oldcode,patch) = syncVisitor.insertInsertDeclareAndInstStatement(rewriter,sblock,insert)
        val description = (
          "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
            + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, InsAfter, patch, start, start, description, Globals.defCost))
      case (_,_ )=>
        println("No InsertDeclareAndInst patch could be generated. " )
        None
    }
  }

  /* debugger */
  def generateInsertDeclareAndInstPatch(insert: InsertDeclareAndInst,
                          ast: ASTStoreElem): Option[PatchBlock] = {
    val res = generateInsertDeclareAndInstPatch_def(insert, ast)
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
                 patches: GroupByIdPatchOptions,
                 patchStore: PatchStore): Unit = {
    try {
      val patches0 = patches.map(patch_id)
      patchStore.update(patch_id, patches)
      patches0.foreach( x => {
        val rewriter = x.rewriter
        x.kind match{
          case Replace   => rewriter.replace(x.start, x.stop, x.patch)
          case InsBefore => rewriter.insertBefore(x.start,x.patch)
          case InsAfter  => rewriter.insertAfter(x.stop,x.patch)
        }
      })
    } catch {
      case _ => println("Invalid patch ID")
    }
  }

  def applyPatch_helper(
                     patch_id_str: String,
                     patches: GroupByIdPatchOptions,
                     patchStore: PatchStore): Unit = {
    try {
      val patch_id = patch_id_str.toInt
      applyPatch_def(patch_id, patches, patchStore)
    } catch {
      case _: Exception => {println("Invalid patch ID")
        None
      }
    }
  }

  /* debugger */
  def applyPatch( patch_id_str: String,
                  patches: GroupByIdPatchOptions,
                  patchStore: PatchStore): Unit = {
    if (false){
      println("Patch id: " + patch_id_str)
      println("Patches: "  + patches)
    }
    applyPatch_helper(patch_id_str, patches, patchStore)
  }

  /* Generates a list of patches corresponding to the list of UPDATE objects (updates) */
  def generateUpdatePatch0(
                            updates: UpdateSync,
                            ast: ASTStoreElem,
                            id: Option[Int] = None): Patch = {
    val res     = generateUpdatePatch(updates,ast)
    val patchID = id match {
      case None => RacerDFix.patchIDGenerator
      case Some (id) => id
    }
    res match {
      case None => NoPatch
      case Some(ptc) => PUpdate(patchID,ptc)
    }
  }


  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateInsertPatch0(
                            inserts: InsertSync,
                            ast: ASTStoreElem,
                            id: Option[Int] = None): Patch = {
     val res     = generateInsertPatch(inserts,ast)
     val patchID = id match {
       case None => RacerDFix.patchIDGenerator
       case Some (id) => id
     }
    res match {
      case None => NoPatch
      case Some(ptc) => PInsert(patchID, ptc)
    }
  }


  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateInsertDeclareAndInstPatch0(
                            inserts: InsertDeclareAndInst,
                            ast: ASTStoreElem,
                            id: Option[Int] = None): Patch = {
    val res     = generateInsertDeclareAndInstPatch(inserts,ast)
    val patchID = id match {
      case None => RacerDFix.patchIDGenerator
      case Some (id) => id
    }
    res match {
      case None => NoPatch
      case Some(ptc) => PInsert(patchID, ptc)
    }
  }

  def generatePatches(fixobj: FixKind,
                      ast: ASTStoreElem,
                      id: Option[Int] = None): Patch = {
    fixobj match {
      case NoFix => NoPatch
      case InsertSync(_,_,_,_)  => generateInsertPatch0(fixobj.asInstanceOf[InsertSync],ast,id)
      case UpdateSync(_,_,_,_)  => generateUpdatePatch0(fixobj.asInstanceOf[UpdateSync],ast,id)
      case InsertDeclareAndInst(_,_,_,_) => generateInsertDeclareAndInstPatch0(fixobj.asInstanceOf[InsertDeclareAndInst],ast,id)
      case And(left, right) =>
        val fresh_id =  id match {
          case None => Some(RacerDFix.patchIDGenerator)
          case Some(_) => id
        }
        new PAnd(generatePatches(left,ast,fresh_id), generatePatches(right,ast,fresh_id))
      case Or(left, right)  => new POr(generatePatches(left,ast,id), generatePatches(right,ast,id))
    }
  }


  /* group patches by ID */
  def generateGroupPatches(groupByIdPatchOptions: GroupByIdPatchOptions,
                           patches: Patch, summ: FSumm): GroupByIdPatchOptions = {
    patches match {
      case NoPatch => groupByIdPatchOptions
      case PInsert(id, block) => {
        groupByIdPatchOptions.update(id,block)
        groupByIdPatchOptions
      }
      case PUpdate(id, block) => {
        groupByIdPatchOptions.update(id,block)
        groupByIdPatchOptions
      }
      case PAnd(left, right)  => {
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left,summ)
        generateGroupPatches(grp1,right,summ)
      }
      case POr(left, right)  => {
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left,summ)
        generateGroupPatches(grp1,right,summ)
      }
    }
  }

  def leastCostlyPatch(groupByIdPatchResult: GroupByIdPatchOptions) = {
    val patch_id = groupByIdPatchResult.map.foldLeft((None:Option[Int],Globals.maxCost))((acc:(Option[Int],PatchCost),pair) => {
      val leastCostlySoFar   = acc._2
      val costOfCurrentPatch = groupByIdPatchResult.getCost(pair._1)
      if (leastCostlySoFar.compare(costOfCurrentPatch) <= 0) acc
      else (Some(pair._1),costOfCurrentPatch)
    })
    patch_id._1
  }

  def translateRawSnapshotsToSnapshots(csumm1: RFSumm, csumm2: Option[RFSumm], ast: ASTManipulation): (FSumm, Option[FSumm]) = {
    csumm2 match {
      case None => {
        val javaFile = csumm1.filename
        val astElem = ast.retrieveAST(javaFile)
        val summ1 = new FSumm(astElem, csumm1)
        (summ1, None)
      }
      case Some(csumm2) => {
          val astElem1 = ast.retrieveAST(csumm1.filename)
          val astElem2 = ast.retrieveAST(csumm2.filename)
          val summ1 = new FSumm(astElem1, csumm1)
          val summ2 = new FSumm(astElem2, csumm2)
          (summ1, Some(summ2))
      }
    }
  }

  def mainAlgo_def(csumm1: RFSumm, csumm2: Option[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore): Unit = {
    /* ******** */
    /*   PARSE  */
    val (summ1,summ2) = translateRawSnapshotsToSnapshots(csumm1, csumm2, ast)

    println("************* GENERATE PATCH *************")
//    if ( true /*csumm1.resource == csumm2.resource*/) {
    var empty_map = new GroupByIdPatchOptions(HashMap.empty[Int, List[PatchBlock]])
    summ2 match {
      case None => {
        /* UNPROTECTED WRITE */
        /* `csumm1` is not synchronized */

        val patch_id = RacerDFix.patchIDGeneratorRange(0)._2
        var empty_map = new GroupByIdPatchOptions(HashMap.empty[Int, List[PatchBlock]])
        val grouped_patches = if (summ1.csumm.locks.length == 0) {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          val inserts1 = generateInsertObjectOnUnprotectedResource(summ1.csumm)

          /* generate insert patches */
          val patches1 = generatePatches(inserts1, summ1.ast, Some(patch_id))

          val grouped_patches1 = generateGroupPatches(empty_map, patches1, summ1)
          grouped_patches1
        } else empty_map

        println(grouped_patches.getText())

        println("************* GENERATE FIX *************")
        applyPatch_def(patch_id, grouped_patches, patchStore)
      }
      case Some(summ2) => {
        if ((summ1.csumm.locks concat summ2.csumm.locks).length > 0) {
          val common_locks = summ1.csumm.locks intersect summ2.csumm.locks

          /* Both statements are synchronized but there is no common lock ==>
        *  1. UPDATE one of the locks with a lock from the peer statement
        *  2. INSERT a new synchronization over one of the two statements  */

          val patches1 = if (summ1.csumm.locks.length > 0 && summ2.csumm.locks.length > 0 && common_locks.length == 0) {
            /* ************** UPDATES ***************** */
            /* generate update objects */
            /* TODO need to check which locks suit to be changed (e.g. they don't protect other resources) */
            /* currently just generate blindly all possibilities */
            if(false) { // TODO avoid updates for now
              val updates1 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ1.csumm, summ2.csumm))
              val updates2 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ2.csumm, summ1.csumm))

              /* generate update patches */
              val update_patches1 = generatePatches(updates1, summ1.ast)
              val update_patches2 = generatePatches(updates2, summ2.ast)
             }
            /* ************** INSERTS ***************** */
            /* generate insert objects */
            val inserts1 = new Or(NoFix,NoFix).mkOr(generateInsertObjects(summ1.csumm, summ2.csumm))
            val inserts2 = new Or(NoFix,NoFix).mkOr(generateInsertObjects(summ2.csumm, summ1.csumm))

            /* generate inserts patches */
            val insert_patches1 = generatePatches(inserts1, summ1.ast)
            val insert_patches2 = generatePatches(inserts2, summ2.ast)

//            val patches1 = generateGroupPatches(empty_map, update_patches1 ++ insert_patches1, summ1)
//            val patches2 = generateGroupPatches(patches1, update_patches2 ++ insert_patches2, summ2)
            val patches1 = generateGroupPatches(empty_map, insert_patches1, summ1)
            val patches2 = generateGroupPatches(patches1, insert_patches2, summ2)
            patches2
          } else empty_map

          /* The resource in `summ1` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `summ2` to protect the
       * statement manipulating the resource in `csumm1`  */

          val patches2 = if (summ1.csumm.locks.length == 0 && summ2.csumm.locks.length > 0) {
            /* ************** INSERTS ***************** */
            /* generate insert objects */
            val inserts = new Or(NoFix,NoFix).mkOr(generateInsertObjects(summ2.csumm, summ1.csumm))

            /* generate insert patches */
            val patches = generatePatches(inserts, summ1.ast)
            generateGroupPatches(patches1, patches, summ1)
          } else patches1

          /* The resource in `csumm2` is not synchronized but that in `csumm1` is ==>
       * INSERT a new synchronization using locks from `csumm` to protect the
       * statement manipulating the resource in `csumm2`  */

          val patches3 = if (summ1.csumm.locks.length > 0 && summ2.csumm.locks.length == 0) {
            /* ************** INSERTS ***************** */
            /* generate insert objects */
            val inserts = new Or(NoFix,NoFix).mkOr(generateInsertObjects(summ1.csumm, summ2.csumm))

            /* generate insert patches */
            val patches = generatePatches( inserts, summ2.ast)
            generateGroupPatches(patches2, patches, summ2)
          } else patches2

          val grouped_patches = patches3

          if (config.interactive) {
            /* when working in interactive mode allow the user to choose the patch */
            println(grouped_patches.getText())

            println("Choose a patch <enter patch id>")
            val patch_id_str = readLine()

            println("************* GENERATE FIX *************")
            applyPatch(patch_id_str, grouped_patches, patchStore)
          } else {
            println(grouped_patches.getText())
            /* apply the patch with the least cost */
            val patch_id = leastCostlyPatch(grouped_patches)
            patch_id match {
              case None =>
              case Some(id) => applyPatch_def(id, grouped_patches, patchStore)
            }
          }
        } else {

          /* Both of the resources `csumm1` and `csumm2` are not synchronized ==>
           * INSERT new synchronization using a lock on the common resource  */

          val patch_id = RacerDFix.patchIDGeneratorRange(0)._2
          var empty_map = new GroupByIdPatchOptions(HashMap.empty[Int, List[PatchBlock]])
          val grouped_patches = if (summ1.csumm.locks.length == 0 && summ2.csumm.locks.length == 0) {
            /* ************** INSERTS ***************** */
            /* generate insert objects */
            val (inserts1, inserts2) = generateInsertObjectOnCommonResource(summ1.csumm, summ2.csumm)
            val ins1 = new Or(NoFix,NoFix).mkOr(inserts1)
            val ins2 = new Or(NoFix,NoFix).mkOr(inserts2)


            /* generate insert patches */
            val patches1 = generatePatches(ins1, summ1.ast, Some(patch_id))
            val patches2 = generatePatches(ins2, summ2.ast, Some(patch_id))

            val grouped_patches1 = generateGroupPatches(empty_map, patches1, summ1)
            val grouped_patches2 = generateGroupPatches(grouped_patches1, patches2, summ2)
            grouped_patches2
          } else empty_map

          println(grouped_patches.getText())

          println("************* GENERATE FIX *************")
          applyPatch_def(patch_id, grouped_patches, patchStore)
        }
      }
    }
  }

  /* logging wrapper for mainAlgo */
  def mainAlgo(csumm1: RFSumm, csumm2: Option[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore): Unit = {
    def fnc (a: Unit) = mainAlgo_def(csumm1: RFSumm, csumm2: Option[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore)
    Logging.addTime("Time to generate patch: ", fnc, ())
  }
}

/**
 * TODO re-implement the cost function and choose a patch accordingly
 * TODO check how to avoid insertion of new lines with rewriter
 * TODO create a set of variables for all possible combinations: {this.A, A} ... what about the fields access?
 * TODO slice the variable declaration for INSERT
 * TODO add a logging mechanisms to keep track of what patches have been applied to which files
 *
 * */