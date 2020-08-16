package org.racerdfix.utils

import org.racerdfix.GroupByIdPatchOptions

import scala.collection.mutable


class PatchStoreElem(val choiceId: Int, val patches: GroupByIdPatchOptions)
/* could have used the hash as a key but in case we need to change this in future */
class PatchStoreKey(val bugHash: String) { def getText() = bugHash }

class PatchStore{
  val map = new mutable.HashMap[String,PatchStoreElem]()
  var bug = ""

  def update(choice: Int, patches: GroupByIdPatchOptions) = {
    map.update(new PatchStoreKey(bug).getText, new PatchStoreElem(choice, patches))
  }
}
