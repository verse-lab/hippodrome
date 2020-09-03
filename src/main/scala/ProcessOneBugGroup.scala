package org.racerdfix

import org.racerdfix.language.{And, FSumm, FixKind, InsAfter, InsBefore, InsertDeclareAndInst, InsertSync, Lock, MergeFixes, MergePatchWithInserts, MergeTwoInserts, NoFix, NoPatch, Or, PAnd, PInsert, POr, PTest, PUpdate, Patch, PatchBlock, PatchCost, RFSumm, Replace, Test, UpdateSync, UpdateVolatile, Variable}
import org.antlr.v4.runtime.TokenStreamRewriter
import org.racerdfix.antlr.Java8Parser
import org.racerdfix.inferAPI.RacerDAPI
import utils.{ASTManipulation, ASTStoreElem, Logging, PatchStore}

import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap

/* maps patch ID -> list of individual code modifications */
class OnePatch(val prio: Int, val patch: List[PatchBlock])
class GroupByIdPatchOptions(var map : HashMap[String, OnePatch]) {

  def update(id: String, prio: Int = 0 , pb: PatchBlock): Unit = {
    try {
      this.map.update(id, new OnePatch(prio, this.map(id).patch ++ List(pb)))
    } catch {
      case e => {
        this.map(id) = new OnePatch(prio, List(pb))
      }
    }
  }

  def emptyMap() = HashMap.empty[String, OnePatch]

  def getText(): String = {
    map.foldLeft("")((acc, x) => acc + (
      "\n" +
        "==========================" +
        "\n" +
        "Patch ID: " + x._1 + x._2.patch.foldLeft("")((acc, y) => acc + "\n" + y.description)))
  }

  /* returns the cost of the patch with id `patchID` */
  def getCost(patchID: String) = {
    this.map(patchID).patch.foldLeft(Globals.unitCost)((acc,pb) => acc.add(pb.cost))
  }

  def removeRedundant(patchStore:PatchStore) = {
    /* filter redundant components for each patch */
    this.map.foreach(patch => {
      val patch_components = patch._2.patch
      val filtered_patch = patch_components.foldLeft[List[PatchBlock]](Nil)((acc, pb) => {
        if (
        /* remove duplicated/redundant patch components from within the same patch for the same bug group*/
        acc.exists(pb_acc => pb_acc.equals(pb) || pb_acc.subsumes(pb)) ||
          /* remove components which are subsumed by other patches' for the same bug group */
          /* pb_inner.equals(pb)  || pb_inner.overlaps(pb)*/
          (this.map.exists(patch_inner => patch_inner._1 != patch._1 && patch_inner._2.patch.exists(pb_inner => pb_inner.equals(pb)))) ||
          /* remove components which are subsumed by other patches' for a different bug group */
          patchStore.map.exists((bug_grp) => bug_grp._2.patches.map(bug_grp._2.choiceId).patch.exists(p => p.subsumes(pb) || p.overlaps(pb)))
          ) {
          //println(" Removing redundant or overlapping patch component: \n ######### " + pb.description + "\n ######### ")
          Logging.add(" Removing redundant or overlapping patch component from patch " + patch._1 + ": \n ######### " + pb.description + "\n ######### ")
          acc
        }
          else acc ++ List(pb)
      })
      this.map.update(patch._1, new OnePatch(patch._2.prio,filtered_patch))
    })

    /* remove empty fixes */
    this.map.foreach(patch => {
      val patch_components = patch._2.patch
      patch_components match {
        case Nil => this.map.remove(patch._1)
        case _ =>
      }
    })
  }

  def highestPrio() = {
    val res = this.map.foldLeft[(Int,Option[String])]((-1,None))((acc,one_patch) => {
      acc._2 match {
        case None => (one_patch._2.prio,Some(one_patch._1))
        case Some(id) => if(acc._1 < one_patch._2.prio) (one_patch._2.prio,Some(one_patch._1)) else acc
      }
    })
    res._2
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

  /* Generates a list of UPDATE objects meant to update the locks in
* `form_summ` with locks in `to_summ` */
  def generateUpdateToVolatileObject(summ: FSumm): UpdateVolatile = {
    UpdateVolatile(summ,summ.csumm.cls,summ.csumm.line,summ.csumm.resource,Nil,"","")
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
        println("No UPDATE patch could be generated for attempt ID: " )
        Logging.add("No UPDATE patch could be generated -- " + update.line + " " + update.lock_new)
        None
    }
  }


  def generateUpdateToVolatilePatch(update: UpdateVolatile,
                          ast: ASTStoreElem): Option[PatchBlock] ={
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(update)
    syncVisitor.visit(ast.tree)
    val sblock    = syncVisitor.getTargetCtx
    val modifiers = syncVisitor.getModifiers
    sblock match {
      case Some(sblock) =>
        try {
          val (oldcode, patch) = syncVisitor.updateListOfModifiers(rewriter, sblock.asInstanceOf[Java8Parser.FieldDeclarationContext], update)
          //        println("Patch ID: " + id)
          val description = (
            "Replace (UPDATE) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
              + ("\n" + "-: " + oldcode)
              + ("\n" + "+: " + patch))
          Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.volatileCost, modifiers))
        } catch {
          case _ => {
            println("No VOLATILE patch could be generated (check the type of sblock)")
            Logging.add("No VOLATILE patch could be generated -- " + update.line + " " + update.variable)
            None
          }
        }
      case None =>
        println("No VOLATILE_2 patch could be generated. " )
        Logging.add("No VOLATILE_2 patch could be generated -- " + update.line + " " + update.variable)
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
    val vb      = new Variable(summ.csumm.resource.cls,modifiers, Globals.def_obj_typ, varName, List(varName))
    val declareObj = InsertDeclareAndInst(summ, summ.csumm.line, vb, modifiers)
    val insert1 = InsertSync(summ,summ.csumm.cls,summ.csumm.line,summ.csumm.resource,vb)
    And(declareObj,insert1)
  }


  def generateInsertPatch_def(insert: InsertSync,
                              ast: ASTStoreElem): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(insert)
    syncVisitor.visit(ast.tree)
    val modifiers = syncVisitor.getModifiers
    val sblock = syncVisitor.getTargetCtx
    val sdecl  = syncVisitor.getDeclSlice
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert,sdecl)
        val description = (
        "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
          + ("\n" + "-: " + oldcode)
          + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost, modifiers, Some(sblock)))
      case None =>
        println("No INSERT patch could be generated for attempt ID " )
        Logging.add("No INSERT patch could be generated -- " + insert.line + " " + insert.resource + " on " + insert.lock)
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

  def generateMergedPatchForTwoInserts_def(insert1: InsertSync,
                              insert2: InsertSync,
                              ast: ASTStoreElem): Option[PatchBlock] = {
    val syncVisitor = new SynchronizedVisitor
    val rewriter    = new TokenStreamRewriter(ast.tokens)
    syncVisitor.setFix(MergeTwoInserts(insert1,insert2))
    syncVisitor.visit(ast.tree)
    val modifiers = syncVisitor.getModifiers.distinct
    val sblock = syncVisitor.getTargetCtx
    val sdecl  = syncVisitor.getDeclSlice
    sblock match {
      case Some(sblock) =>
        val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,sblock,insert1,sdecl)
        val description = (
          "Replace (INSERT) lines: " + Globals.getRealLineNo(sblock.start.getLine) + " - " + Globals.getRealLineNo(sblock.stop.getLine)
            + ("\n" + "-: " + oldcode)
            + ("\n" + "+: " + patch) )
        Some(new PatchBlock(ast.rewriter, Replace, patch, sblock.start, sblock.stop, description, Globals.defCost, modifiers))
      case None =>
        println("No INSERT patch could be generated for attempt ID " )
        Logging.add("No Merge patch could be generated -- " + insert1.line + " " + insert1.resource + " on " + insert1.lock + " , " +
          insert2.line + " " + insert2.resource + " " + insert2.lock)
        None
    }
  }

  def generateMergedPatch_def(lst: MergeFixes): List[Option[PatchBlock] ]= {

    val patches     = lst.fixes.foldLeft(List.empty[Option[PatchBlock]])((acc,ins) => {
      val ast         = ins.fsumm.ast
      val syncVisitor = new SynchronizedVisitor
      val rewriter    = new TokenStreamRewriter(ast.tokens)
      if(acc.isEmpty) List(generateInsertPatch_def(ins,ast))
      else {
        val patch = acc(acc.length-1)
        val new_acc = acc.dropRight(1)
        patch match {
          case None => {
            /* just generate the patch for ins and ignore this none */
            generateInsertPatch_def(ins,ast) match {
              case None => new_acc
              case Some(fresh_patch) => new_acc ++ List(Some(fresh_patch))
            }
          }
          case Some(patch) => {
            /* merge the patch with the insert */
            syncVisitor.setFix(MergePatchWithInserts(patch,ins))
            syncVisitor.visit(ast.tree)
            val modifiers = syncVisitor.getModifiers.distinct
            val sblock = syncVisitor.getSurroundingStmt
            val sdecl  = syncVisitor.getDeclSlice
            sblock match {
              case (Some(start), Some(stop)) =>
                val (oldcode,patch) = syncVisitor.insertSynchronizedStatement(rewriter,start,ins,sdecl, start_ctx = Some(start), stop_ctx = Some(stop))
                val description = (
                  "Replace (INSERT) lines: " + Globals.getRealLineNo(start.start.getLine) + " - " + Globals.getRealLineNo(stop.stop.getLine)
                    + ("\n" + "-: " + oldcode)
                    + ("\n" + "+: " + patch) )
                new_acc ++ List(Some(new PatchBlock(ast.rewriter, Replace, patch, start.start, stop.stop, description, Globals.defCost, modifiers)))
              case _ =>
                println("No INSERT patch could be generated for attempt ID " )
                Logging.add("No Merge patch could be generated -- " + ins.line + " " + ins.resource + " on lock " + ins.lock + " , " )
                acc ++ List (generateInsertPatch_def(ins,ast) )
            }
          }
        }
      }
    })
    patches
  }

  /* debugger */
  def generateMergedPatch(lst: MergeFixes): List[Option[PatchBlock] ]= {
    val res = generateMergedPatch_def(lst)
    if(true) {
      println(lst)
      println("out: " + Globals.print_list(Globals.getTextOpt[PatchBlock],",", res))
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
        Logging.add("No InsertDeclareAndInst patch could be generated -- " + insert.line + " " + insert.variable.toString )
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

  def getVariablesStore(ast: ASTStoreElem): mutable.HashMap[String, List[Variable]] = {
    val syncVisitor = new SynchronizedVisitor
    syncVisitor.visit(ast.tree)
    syncVisitor.variables
  }


  /* ************************************************ */
  /*                         FIX                      */
  /* ************************************************ */

  def applyPatch_def(
                 patch_id: String,
                 patches: GroupByIdPatchOptions,
                 patchStore: PatchStore): Unit = {
    try {
      println("Applying Patch ID: " + patch_id)
      val patches0 = patches.map(patch_id)
      patchStore.update(patch_id, patches)
      patches0.patch.foreach( x => x.rewriter.addInstruction(x.kind, x.start, x.stop, x.patch))
    } catch {
      case _ => println("Invalid patch ID")
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
    applyPatch_def(patch_id_str, patches, patchStore)
  }

  /* Generates a list of patches corresponding to the list of UPDATE objects (updates) */
  def generateUpdatePatch0(
                            updates: UpdateSync,
                            ast: ASTStoreElem,
                            id: Option[String] = None): Patch = {
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
                            id: Option[String] = None): Patch = {
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
  def generateMergePatch0(
                            inserts: MergeFixes,
                            id: Option[String] = None): Patch = {
    val res     = generateMergedPatch_def(inserts)
    val patchID = id match {
      case None => RacerDFix.patchIDGenerator
      case Some (id) => id
    }

    def patchBlockToPatch(patch: Option[PatchBlock]) = {
      patch match {
        case None => NoPatch
        case Some(patch) => PInsert(patchID, patch)
      }
    }

    res match {
      case Nil    => NoPatch
      case x::Nil => patchBlockToPatch(x)
      case x::xs  => {
        xs.foldLeft(patchBlockToPatch(x))((acc,p) => PAnd(patchID,acc,patchBlockToPatch(p)))
      }
    }
  }

  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateInsertDeclareAndInstPatch0(
                            inserts: InsertDeclareAndInst,
                            ast: ASTStoreElem,
                            id: Option[String] = None): Patch = {
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


  /* Generates a list of patches corresponding to the list of INSERT objects (inserts) */
  def generateUpdateToVolatilePatch0(
                                          update: UpdateVolatile,
                                          ast: ASTStoreElem,
                                          id: Option[String] = None): Patch = {
    val res     = generateUpdateToVolatilePatch(update,ast)
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
                      id: Option[String] = None): Patch = {
    fixobj match {
      case NoFix   => NoPatch
      case Test(_) => NoPatch
      case MergeFixes(lst) => generateMergePatch0(fixobj.asInstanceOf[MergeFixes],id)
      case InsertSync(s,_,_,_,_)  => generateInsertPatch0(fixobj.asInstanceOf[InsertSync],s.ast,id)
      case UpdateSync(s,_,_,_,_)  => generateUpdatePatch0(fixobj.asInstanceOf[UpdateSync],s.ast,id)
      case UpdateVolatile(s, _, _, _, _, _, _) => generateUpdateToVolatilePatch0(fixobj.asInstanceOf[UpdateVolatile],s.ast,id)
      case InsertDeclareAndInst(s,_,_,_) => generateInsertDeclareAndInstPatch0(fixobj.asInstanceOf[InsertDeclareAndInst],s.ast,id)
      case And(left, right) =>
        val fresh_id =  id match {
          case None =>    RacerDFix.patchIDGenerator
          case Some(id) => id
        }
        (generatePatches(left, Some(fresh_id)), generatePatches(right,Some(fresh_id))) match {
//          case (NoPatch, _) => NoPatch
//          case (_,NoPatch)  => NoPatch
          case (p1, p2)     => new PAnd(fresh_id, p1, p2)
        }
      case Or(left, right)  => new POr(RacerDFix.patchIDGenerator, generatePatches(left,Some(RacerDFix.patchIDGenerator)), generatePatches(right,Some(RacerDFix.patchIDGenerator)))
    }
  }


  /* group patches by ID */
  def generateGroupPatches(groupByIdPatchOptions: GroupByIdPatchOptions,
                           patches: Patch): GroupByIdPatchOptions = {
    patches match {
      case NoPatch => groupByIdPatchOptions
      case PInsert(id, block) => {
        groupByIdPatchOptions.update(id,pb=block)
        groupByIdPatchOptions
      }
      case PUpdate(id, block) => {
        groupByIdPatchOptions.update(id,pb=block)
        groupByIdPatchOptions
      }
      case PAnd(_,left, right)  => {
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left)
        generateGroupPatches(grp1,right)
      }
      case POr(_,left, right)  => {
        val grp1 = generateGroupPatches(groupByIdPatchOptions,left)
        generateGroupPatches(grp1,right)
      }
    }
  }

/*
  def leastCostlyPatch(groupByIdPatchResult: GroupByIdPatchOptions) = {
    val patch_id = groupByIdPatchResult.map.foldLeft((None:Option[String],Globals.maxCost))((acc:(Option[String],PatchCost),pair) => {
      val leastCostlySoFar   = acc._2
      val costOfCurrentPatch = groupByIdPatchResult.getCost(pair._1)
      if (leastCostlySoFar.compare(costOfCurrentPatch) <= 0) acc
      else (Some(pair._1),costOfCurrentPatch)
    })
    patch_id._1
  }
*/

  def leastCostlyPatch(groupByIdPatchResult: GroupByIdPatchOptions) = {
    val patch_id = groupByIdPatchResult.map.foldLeft((None:Option[String]))((acc:(Option[String]),pair) => {
      try {
        acc match {
          case None => Some(pair._1)
          case Some(id) => if (id.toInt <= pair._1.toInt) acc else Some(pair._1)
        }
      } catch {
        case _ => {
          println("Found ID which is not Int. ")
          acc
        }}
    })
    patch_id
  }


  def translateRawSnapshotsToSnapshots(csumms: List[RFSumm], ast: ASTManipulation): List[FSumm] = {
    csumms.map(csumm => {
      val astElem = ast.retrieveAST(csumm.filename)
      val summ    = new FSumm(astElem, csumm)
      summ
    })
  }

  /* ******************************** */
  /*  Functions for merging patches   */
  /* ******************************** */
  def compatibleForMerging(f1: FixKind, f2: FixKind): Boolean = {
    (f1,f2) match {
      case (InsertSync(fsum1,cls1,_,_,lock1), InsertSync(fsum2,cls2,_,_,lock2)) =>
        if(fsum1.csumm.procedure == fsum2.csumm.procedure && cls1 == cls2 && lock1.equals_loose(lock2))
          true
        else false
      case (MergeFixes(lst), InsertSync(_,_,_,_,_)) =>
        if(lst.isEmpty) true
        else compatibleForMerging(lst(lst.length-1),f2)
      case _ => false
    }
  }

  /* order sensitive - MergeFixes must be the first */
  def mergeFixes(f1: FixKind, f2: FixKind): List[FixKind] = {
    (f1,f2) match {
      case (InsertSync(fsum1,cls1,_,_,lock1), InsertSync(fsum2,cls2,_,_,lock2)) =>
        if(compatibleForMerging(f1,f2))
          List(MergeFixes(List(f1.asInstanceOf[InsertSync],f2.asInstanceOf[InsertSync])))
        else List(f1,f2)
      case (MergeFixes(lst), InsertSync(_,_,_,_,_)) =>
        if(lst.isEmpty) List(f2)
        else if(compatibleForMerging(lst(lst.length-1),f2)) List(MergeFixes(lst ++ List(f2.asInstanceOf[InsertSync])))
        else List(f1,f2)
      case _ => List(f1,f2)
    }
  }

  def possiblyMergeFixes(fix: FixKind) = {
    val insert_lst = fix.asInstanceOf[And].listOf()
    val insert_lst1 = insert_lst.sortBy(f => try {
      (f.asInstanceOf[InsertSync].fsumm.csumm.procedure, f.asInstanceOf[InsertSync].fsumm.csumm.line)
    } catch {
      case _ => ("", 0) // if not an insert then it's not grouped
    })
    val insert_lst2 = insert_lst1.foldLeft(List.empty[FixKind])((acc, fix) => {
      acc match {
        case Nil => List(fix)
        case _ => {
          val last_fix = acc(acc.length - 1)
          val new_acc = acc.dropRight(1)
          /* possibly merge fixes */
          val fixes = mergeFixes(last_fix, fix)
          new_acc ++ fixes
        }
      }
    })
    val inserts = insert_lst2 match {
      case Nil => NoFix
      case x :: Nil => x
      case x :: xs => xs.foldLeft(x)((acc, fix) => And(acc, fix))
    }
    inserts
  }

  def possiblyMergeFixesOpt(fixes: FixKind): FixKind = {
    fixes match {
      case Or(p1,p2) => new Or(possiblyMergeFixesOpt(p1),possiblyMergeFixesOpt(p2))
      case And(_,_ ) => possiblyMergeFixes(fixes)
      case _         => fixes
    }
  }
  /**/

  def mainAlgo_def(csumms: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore): Option[(String, GroupByIdPatchOptions)]  = {
    /* ******** */
    /*   PARSE  */
    val summs = translateRawSnapshotsToSnapshots(csumms, ast)

    println("************* GENERATE PATCH *************")
    //    if ( true /*csumm1.resource == csumm2.resource*/) {
    var empty_map = new GroupByIdPatchOptions(HashMap.empty[String, OnePatch])
    summs match {
      case Nil => None
      case summ::Nil => {
        /* UNPROTECTED WRITE */
        val patch_id = RacerDFix.patchIDGeneratorRange(0)._2
        var empty_map = new GroupByIdPatchOptions(HashMap.empty[String, OnePatch])
        val grouped_patches = if (summ.csumm.locks.length == 0) {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          val inserts1 = generateInsertObjectOnUnprotectedResource(summ)

          /* generate insert patches */
          val patches = generatePatches(inserts1, Some(patch_id.toString))

          /* generate volatile */
          val update      = generateUpdateToVolatileObject(summ)
          val aux_patches = generatePatches(update, Some(RacerDFix.patchIDGenerator()))

          val grouped_patches1 = generateGroupPatches(empty_map, patches)
          val grouped_patches  = generateGroupPatches(grouped_patches1, aux_patches)

          grouped_patches
        } else empty_map

        grouped_patches.removeRedundant(patchStore)
        println(grouped_patches.getText())

        if (config.interactive) {
          println("Choose a patch <enter patch id>")
          val patch_id_str = readLine()

          println("************* GENERATE FIX *************")
          // applyPatch(patch_id_str, grouped_patches, patchStore)
          Some (patch_id_str, grouped_patches)
        } else {
          val patch_id = leastCostlyPatch(grouped_patches)
          println("Applying Patch ID " + patch_id)
          patch_id match {
            case None     => None
            case Some(id) => Some(id, grouped_patches)
            /* applyPatch(id, grouped_patches, patchStore) */
          }
        }
      }
      case _ => {
        /* retrieve possible modifiers e.g. static */
        val modifiers        = summs.foldLeft[List[String]](Nil)((acc,p) => acc ++ generateTestPatch(new Test(p.csumm.line),p.ast).block.modifiers).distinct
        val atLeastOneStatic = modifiers.exists( m => m == "static")
        val variables_store  = getVariablesStore(summs.head.ast)
        def getStaticVars(cls: String): List[Variable] = variables_store.getOrElseUpdate(cls,Nil).filter(v => v.isStatic())
        def isLockStatic(lck: Lock) = {
          val static_vars = getStaticVars(lck.cls)
//          static_vars.exists( p => RacerDAPI.refToListOfRef(lck.resource.id).contains(p.id))
          static_vars.exists( p => lck.resource.allAliases.contains(p.id))
        }

        val existing_locks   = summs.foldLeft[List[Lock]](Nil)((acc,summ) => {
          val locks = summ.csumm.locks.filter( lck => (atLeastOneStatic &&  isLockStatic((lck)) || !atLeastOneStatic ))
          acc ++ locks
        })

        val existing_locks_ext  = existing_locks.map( lock => (lock,summs.count( p => p.csumm.locks.exists(lck => lck.equals(lock)))))

        /* sorts locks based on their occurrence */
        val candidate_locks = existing_locks_ext.sortBy(lck => -lck._2)

        def create_new_lock : (FixKind, Lock)=   {
          /* CREATE new lock object */
          val varName = Globals.base_obj_id + RacerDFix.patchIDGenerator
          /* TODO below assumes that all bugs are in the same class */
          val varb    = new Variable(summs.head.csumm.resource.cls,modifiers,Globals.def_obj_typ,varName,List(varName))
          val declareObj = InsertDeclareAndInst(summs.head,summs.head.csumm.line, varb, modifiers)
          (declareObj, new Lock("this",summs.head.csumm.resource.cls,varb))
        }

        val locks = candidate_locks.map(lck => (NoFix.asInstanceOf[FixKind],lck._1)) ++ List(create_new_lock)

        val grouped_patches = {
          /* ************** INSERTS ***************** */
          /* generate insert objects */
          // println("LOCK: " + lock.resource)
          val insert = locks.foldLeft(NoFix.asInstanceOf[FixKind])( (fxk, lck) => {
            val ptch =
            summs.foldLeft(lck._1: FixKind)((acc, p) =>
              new And(acc, (generateInsertObjects(p, lck._2))))
            new Or(fxk,ptch)
          })

          val inserts = {
            if(!config.atomicity) insert
            else possiblyMergeFixesOpt(insert)
              /* sort the above based on method and merge patches belonging to the same method */
              /* merge objects which are \delta apart or which are within the same method */
          }
          /* generate inserts patches */
          val insert_patches = generatePatches(inserts)

          /* generate volatile */
          val update =  generateUpdateToVolatileObject(summs.head)
          val aux_patches = generatePatches(update, Some(RacerDFix.patchIDGenerator()))

          /* group patches based on their ID */
          val patches0 = generateGroupPatches(empty_map, insert_patches)
          val patches  = generateGroupPatches(patches0, aux_patches)

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
          /* applyPatch(patch_id_str, grouped_patches, patchStore) */
          Some(patch_id_str, grouped_patches)
        } else {
          println(grouped_patches.getText())
          /* apply the patch with the least cost */
          val patch_id = leastCostlyPatch(grouped_patches)
          println("Applying Patch ID " + patch_id)
          patch_id match {
            case None     => None
            case Some(id) => Some(id, grouped_patches)
            /* applyPatch(id, grouped_patches, patchStore)*/
          }
        }
      }
    }
  }

  /* logging wrapper for mainAlgo */
  def mainAlgo(csumm: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore) : Option[(String, GroupByIdPatchOptions)]  = {
    def fnc (a: Unit): Option[(String, GroupByIdPatchOptions)]  = mainAlgo_def(csumm: List[RFSumm], config: FixConfig, ast: ASTManipulation, patchStore: PatchStore)
    Logging.addTime("Time to generate patch: ", fnc, ())
  }
}

/**
 * TODO re-implement the cost function and choose a patch accordingly
 * TODO check how to avoid insertion of new lines with rewriter
 * TODO create a set of variables for all possible combinations: {this.A, A} ... what about the fields access?
 * TODO: throw an exception when one patch_component cannot be created
 * */