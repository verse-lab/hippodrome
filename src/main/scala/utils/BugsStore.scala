package org.racerdfix.utils

import org.racerdfix.language.{FBug, FSumm, RFSumm}

import scala.collection.mutable
import scala.collection.mutable.HashMap

class BugsStore {
  val map: HashMap[String, (List[String],List[FBug])] = new mutable.HashMap[String, (List[String],List[FBug])]

  /* TODO this is monstrous - need to refactor */
  def update(bug: FBug) = {
    val snapshots = bug.snapshot1 ++ bug.snapshot2

    /* assume all snapshots refer to the same resource. */
    if(snapshots.length > 0){
      try {
        val resources = snapshots.map(rfsumm => rfsumm.resource.allAliases()).flatten.distinct
        /* at least one of resources is key in map */
        val key = resources.foldLeft[Option[String]](None)( (acc,res) => {
          /* if the resources is already a key keep it as a key */
          if (map.contains(res))  Some(res)
          else {
            /* if the resources is part of existing resources for a certain `key` return that `key` */
            acc match {
              case  None =>
                map.foldLeft[Option[String]](acc)((acc0,mapElem) => {
                  val existing_resources = mapElem._2._1
                  val existing_key  = mapElem._1
                  if(existing_resources.contains(res)) Some(existing_key)
                  else acc0
                })
              case Some(_) => acc
            }
          }
        })
      key match {
        case None => map.update(resources.head,(resources,List(bug)))
        case Some(key) =>
          val (res,bugs) = map(key)
          map.update(key, ((res ++ resources).distinct, (bugs ++ List(bug)).distinct ))
        }
      }
     catch{
        case _ => map.update(bug.hash,  (Nil,List(bug)))
      }
    }
    else map.update(bug.hash,  (Nil,List(bug)))
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
