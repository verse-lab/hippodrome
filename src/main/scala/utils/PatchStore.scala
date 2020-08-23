package org.racerdfix.utils

import org.racerdfix.GroupByIdPatchOptions

import scala.collection.mutable


class PatchStoreElem(val choiceId: String, val patches: GroupByIdPatchOptions)
/* could have used the hash as a key but in case we need to change this in future */
class PatchStoreKey(val bugHash: String) { def getText() = bugHash }

class PatchStore{
  val map = new mutable.HashMap[String,PatchStoreElem]()
  var bug = ""

  def update(choice: String, patches: GroupByIdPatchOptions) = {
    Logging.add("Patch ID:" + choice)
    Logging.add("Patch: " + patches.map(choice).map( pb => pb.toStringDetailed() + "\n"))
    map.update(new PatchStoreKey(bug).getText, new PatchStoreElem(choice, patches))
  }
}
