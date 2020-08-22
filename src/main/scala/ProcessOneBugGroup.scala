package org.racerdfix

import org.racerdfix.language.{And, FSumm, FixKind, InsAfter, InsBefore, InsertDeclareAndInst, InsertSync, Lock, NoFix, NoPatch, Or, PAnd, PInsert, POr, PTest, PUpdate, Patch, PatchBlock, PatchCost, RFSumm, Replace, Test, UpdateSync}
import org.antlr.v4.runtime.TokenStreamRewriter
import utils.{ASTManipulation, ASTStoreElem, Logging, PatchStore}

import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap

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

  def removeRedundant(patchStore:PatchStore) = {
    /* filter redundant components for each patch */
    this.map.foreach(patch => {
      val patch_components = patch._2
      val filtered_patch   = patch_components.foldLeft[List[PatchBlock]](Nil)((acc,pb) => {
        if (
          /* remove duplicated/redundant patch components from within the same patch for the same bug group*/
          acc.exists(pb_acc => pb_acc.equals(pb) || pb_acc.subsumes(pb)) ||
          /* remove components which are subsumed by other patches' for the same bug group */
          (this.map.exists(patch_inner => patch_inner._1 != patch._1 && patch_inner._2.exists(pb_inner => pb_inner.subsumes(pb))))||
          /* remove components which are subsumed by other patches' for a different bug group */
          patchStore.map.exists((bug_grp) => bug_grp._2.patches.map(bug_grp._2.choiceId).exists(p => p.subsumes(pb)))
          ) acc
        else acc ++ List(pb)})
      this.map.update(patch._1,filtered_patch)})
    }
}


object ProcessOneBugGroup  {

  /* ************************************************ */
  /*                       UPDATE                     */
  /* ************************************************ */

  /* Generates a list of UPDATE objects meant to update the locks in
  * `form_summ` with locks in `to_summ` */
  def generateUpdateObjects(from_summ: FSumm, to_summ: RFSumm): List[UpdateSync] = {
    from_summ.csumm.locks.foldLeft(List[UpdateSync]())((acc, lock) => {
      val lck  = lock.resource
      val locks = to_summ.locks.foldLeft(acc)((acc2, lock2) => UpdateSync(from_summ,from_summ.csumm.cls,from_summ.csumm.line,lck,lock2.resource)::acc2)
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
    val modifiers = syncVisitor.getModifiers
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.updateSynchronizedStatement(rewriter,sblock,update)
//        println("Patch ID: " + id)
        val description = (
        "Replace (UPDATE) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
        + ("\n" + "-: " + oldcode)
        + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost, modifiers))
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
  def generateInsertObjects(summ: FSumm, lock: Lock): FixKind = {
    if (summ.csumm.locks.exists(lck => lck.equals(lock))) NoFix
    else InsertSync(summ, summ.csumm.cls, summ.csumm.line, summ.csumm.resource, lock.resource)
   }

  /* Generates a list of INSERT objects for the resource in `resource_summ`
   based on the locks available in summary `locks_summ` */
  def generateInsertObjectOnUnprotectedResource(summ: FSumm) = {
    /* TODO  need to check which is that common resouce, e.g. myA or myA.f?
    *   should be the outer most one, e.g. myA*/
    /* TODO need to recheck what is the resource we create lock for. myA.f is not the right type, it should be
    *   a reference type. */
    val modifiers = generateTestPatch(new Test(summ.csumm.line),summ.ast).block.modifiers.distinct
    val varName = Globals.base_obj_id + RacerDFix.patchIDGenerator
    val declareObj = InsertDeclareAndInst(summ, summ.csumm.cls,summ.csumm.line, Globals.def_obj_typ, varName, modifiers)
    val insert1 = InsertSync(summ,summ.csumm.cls,summ.csumm.line,summ.csumm.resource,varName)
    And(declareObj,insert1)
  }

  def generateInsertPatch_def(insert: InsertSync,
                              ast: ASTStoreElem): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(insert)
    syncVisitor.visit(ast.tree)
    val modifiers = syncVisitor.getModifiers
    val sblock = syncVisitor.getResourceStatement
    val sdecl  = syncVisitor.getDeclSlice
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert,sdecl)
        val description = (
        "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
          + ("\n" + "-: " + oldcode)
          + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost, modifiers))
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
    val modifiers = (syncVisitor.getModifiers ++ insert.modifiers).distinct
    val sdecl  = syncVisitor.getDeclSlice
    (sblock,syncVisitor.getClassStart) match {
      case (Some(sblock),Some(start)) =>
        val insert_modif = insert.clone(modifiers = modifiers)
        val (oldcode,patch) = syncVisitor.insertInsertDeclareAndInstStatement(rewriter,sblock,insert_modif)
        val description = (
          "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
            + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, InsAfter, patch, start, start, description, Globals.defCost, modifiers))
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


  def generateTestPatch(test: Test,
                        ast: ASTStoreElem): PTest = {
    val syncVisitor = new SynchronizedVisitor
    syncVisitor.setFix(test)
    syncVisitor.visit(ast.tree)
    val modifiers = syncVisitor.getModifiers
    PTest(new PatchBlock(ast.rewriter, Replace, "", ast.tokens.get(0), ast.tokens.get(0), "", Globals.defCost, modifiers))
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
                      id: Option[Int] = None): Patch = {
    fixobj match {
      case NoFix   => NoPatch
      case Test(_) => NoPatch
      case InsertSync(s,_,_,_,_)  => generateInsertPatch0(fixobj.asInstanceOf[InsertSync],s.ast,id)
      case UpdateSync(s,_,_,_,_)  => generateUpdatePatch0(fixobj.asInstanceOf[UpdateSync],s.ast,id)
      case InsertDeclareAndInst(s,_,_,_,_,_) => generateInsertDeclareAndInstPatch0(fixobj.asInstanceOf[InsertDeclareAndInst],s.ast,id)
      case And(left, right) =>
        val fresh_id =  id match {
          case None => Some(RacerDFix.patchIDGenerator)
          case Some(_) => id
        }
        new PAnd(generatePatches(left,fresh_id), generatePatches(right,fresh_id))
      case Or(left, right)  => new POr(generatePatches(left,id), generatePatches(right,id))
    }
  }


  /* group patches by ID */
  def generateGroupPatches(groupByIdPatchOptions: GroupByIdPatchOptions,
                           patches: Patch): GroupByIdPatchOptions = {
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
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left)
        generateGroupPatches(grp1,right)
      }
      case POr(left, right)  => {
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left)
        generateGroupPatches(grp1,right)
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

  def translateRawSnapshotsToSnapshots(csumms: List[RFSumm], ast: ASTManipulation): List[FSumm] = {
    csumms.map(csumm => {
      val astElem = ast.retrieveAST(csumm.filename)
      val summ    = new FSumm(astElem, csumm)
      summ
    })
  }

  def mainAlgo_def(csumms: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore): Unit = {
    /* ******** */
    /*   PARSE  */
    val summs = translateRawSnapshotsToSnapshots(csumms, ast)

    println("************* GENERATE PATCH *************")
    //    if ( true /*csumm1.resource == csumm2.resource*/) {
    var empty_map = new GroupByIdPatchOptions(HashMap.empty[Int, List[PatchBlock]])
    summs match {
      case Nil =>
      case summ::Nil => {
        /* UNPROTECTED WRITE */
        /* `csumm1` is not synchronized */

        val patch_id = RacerDFix.patchIDGeneratorRange(0)._2
        var empty_map = new GroupByIdPatchOptions(HashMap.empty[Int, List[PatchBlock]])
        val grouped_patches = if (summ.csumm.locks.length == 0) {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          val inserts1 = generateInsertObjectOnUnprotectedResource(summ)

          /* generate insert patches */
          val patches = generatePatches(inserts1, Some(patch_id))

          val grouped_patches1 = generateGroupPatches(empty_map, patches)
          grouped_patches1
        } else empty_map

        grouped_patches.removeRedundant(patchStore)
        println(grouped_patches.getText())

        //println("************* GENERATE FIX *************")
        applyPatch_def(patch_id, grouped_patches, patchStore)
      }
      case _ => {
        /* retrieve possible modifiers e.g. static */
        val modifiers = summs.foldLeft[List[String]](Nil)((acc,p) => acc ++ generateTestPatch(new Test(p.csumm.line),p.ast).block.modifiers).distinct

        val existing_locks     = summs.foldLeft[List[Lock]](Nil)((acc,summ) => acc ++ summ.csumm.locks)
        val common_locks       = summs.foldLeft[List[Lock]](Nil)((acc,summ) => acc intersect summ.csumm.locks)
        val existing_locks_ext = existing_locks.map( lock => (lock,summs.count( p => p.csumm.locks.exists(lck => lck.equals(lock)))))
        /* TODO start from a new object */
        val candidate_lock     = existing_locks_ext.foldLeft[Option[(Lock,Int)]](None)((acc,lck_ext) => {
          acc match {
            case None => {
              Some(lck_ext)
            }
            case Some (lck_0) => {
              if (lck_ext._2 < lck_0._2) {
                Some(lck_0)
              } else {
                Some(lck_ext)
              }
            }
          }
        })
        val (fix_aux,lock) = {
          candidate_lock match {
            case Some(lck) => (NoFix, lck._1)
            case None      => {
              /* CREATE new lock object */
              val varName = Globals.base_obj_id + RacerDFix.patchIDGenerator
              val declareObj = InsertDeclareAndInst(summs.head,summs.head.csumm.cls,summs.head.csumm.line, Globals.def_obj_typ, varName, modifiers)
              (declareObj, new Lock("this",summs.head.csumm.cls,varName))
            }
          }
        }

        val grouped_patches = {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          // println("LOCK: " + lock.resource)
          val insert = summs.foldLeft(fix_aux:FixKind)((acc,p) => new And(acc,(generateInsertObjects(p,lock))))

          /* generate inserts patches */
          val insert_patches = generatePatches(insert)

          /* group patches based on their ID */
          val patches = generateGroupPatches(empty_map, insert_patches)
          /* remove redundant patches */
          patches.removeRedundant(patchStore)
          patches
        }

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
        //}
      }
    }
  }

  /* logging wrapper for mainAlgo */
  def mainAlgo(csumm: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore): Unit = {
    def fnc (a: Unit) = mainAlgo_def(csumm: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore)
    Logging.addTime("Time to generate patch: ", fnc, ())
  }
}

/**
 * TODO re-implement the cost function and choose a patch accordingly
 * TODO check how to avoid insertion of new lines with rewriter
 * TODO create a set of variables for all possible combinations: {this.A, A} ... what about the fields access?
 * TODO slice the variable declaration for INSERT
 * TODO: throw an exception when one patch_component cannot be created
 * */