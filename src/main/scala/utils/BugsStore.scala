package org.racerdfix.utils

import org.racerdfix.language.{FBug, FSumm, RFSumm}

import scala.collection.mutable
import scala.collection.mutable.HashMap

class BugsStore {
  val map: HashMap[String, List[FBug]] = new mutable.HashMap[String, List[FBug]]

  def update(bug: FBug) = {
    /* TODO: relax the equality to subset */
    val key_0 = bug.snapshot1.foldLeft[Option[String]](None)((acc:Option[String],s) => {
      acc match {
        case None => Some(s.resource)
        case Some(res) =>  if (res == s.resource) Some(res) else None
      }
    })

    val key = bug.snapshot2.foldLeft(key_0)((acc:Option[String],s) => {
      acc match {
        case None => Some(s.resource)
        case Some(res) =>  if (res == s.resource) Some(res) else None
      }
    })

    key match {
      case None => {
        key_0 match {
          case None =>
          case Some(key) =>  {
            try {
              map.update(key, map(key) ++ List(bug))
            } catch {
              case _ => map.update(key, List(bug))
            }
          }
        }
      }
      case Some(key) =>  {
        try {
          map.update(key, map(key) ++ List(bug))
        } catch {
          case _ => map.update(key, List(bug))
        }
      }
    }
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
