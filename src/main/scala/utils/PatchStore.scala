package org.racerdfix.utils

import org.racerdfix.GroupByIdPatchOptions

import scala.collection.mutable


class PatchStoreElem(var choiceId: String, var patches: GroupByIdPatchOptions)
/* could have used the hash as a key but in case we need to change this in future */
class PatchStoreKey(val bugHash: String) { def getText() = bugHash }

class PatchStore{
  val map = new mutable.HashMap[String,PatchStoreElem]()
  var bug = ""

  def update(choice: String, patches: GroupByIdPatchOptions) = {
    Logging.add("Patch ID:" + choice)
    Logging.add("Patch: " + patches.map(choice).patch.map( pb => pb.toStringDetailed() + "\n"))
    map.update(new PatchStoreKey(bug).getText, new PatchStoreElem(choice, patches))
  }

  def getFirstInLine() ={
    val highestPrioId = map(bug).patches.highestPrio()
    highestPrioId match {
      case None => None
      case Some(id) => {
        /* update the choice id */
        this.map.update(bug, new PatchStoreElem(id, this.map(bug).patches))
        highestPrioId
      }
    }
  }

  def removePatch(choiceId: String) = {
    this.map(bug).patches.map.remove(choiceId)
  }

  def getPatches() = this.map(bug).patches

//  def get
}
