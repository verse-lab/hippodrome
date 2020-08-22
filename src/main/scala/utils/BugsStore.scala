package org.racerdfix.utils

import org.racerdfix.language.{FBug, FSumm, RFSumm}

import scala.collection.mutable
import scala.collection.mutable.HashMap

class BugsStore {
  val map: HashMap[String, List[FBug]] = new mutable.HashMap[String, List[FBug]]

  /* TODO this is monstrous - need to refactor */
  def update(bug: FBug) = {
    val snapshots = bug.snapshot1 ++ bug.snapshot2
    if(snapshots.length > 0){
      try {
      val key = snapshots.foldLeft[Option[String]](Some(snapshots.head.resource.head))((acc:Option[String],s) => {
        acc match {
          case None => None
          case Some(res) =>  if (s.resource.contains(res)) Some(res) else None
        }
      })

      key match {
        case None =>
        case Some(key) =>  {
          try {
            map.update(key, map(key) ++ List(bug))
          } catch {
            case _ => map.update(key, List(bug))
          }
        }
      }
    } catch{
        case _ => map.update(bug.hash,  List(bug))
      }
    }
    else map.update(bug.hash,  List(bug))
  }
}

object BugsMisc {

  def costSumm(summ: RFSumm) = {
    summ.trace.length()
  }

  def lessExpensive(val1: Int, val2: Int) = val1 <= val2

  def retrieveSummaryFromSnapshot(snapshot: List[RFSumm]) = {
    val summ = snapshot.foldLeft[Option[RFSumm]](None)((acc,summ) => {
      acc match {
        case None => Some(summ)
        case Some(summ2) => if (lessExpensive(summ.getCost(costSumm), summ2.getCost(costSumm ))) Some(summ) else  acc
      }
    } )
    summ match {
      case Some(summ) => List(summ)
      case None => Nil
    }
  }

  def retrieveSummary(bug: FBug) = {
    val summary1 = retrieveSummaryFromSnapshot(bug.snapshot1)
    val summary2 = retrieveSummaryFromSnapshot(bug.snapshot2)
    summary1 ++ summary2
  }
}
