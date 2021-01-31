package org.racerdfix.utils

import com.sun.tools.doclint.Env.AccessKind
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.{FixConfig, Globals}
import org.racerdfix.language.{AccessKind, EmptyTrace, FBug, FSumm, Lock, RFSumm, Read, Trace, Unk, Variable, Write}
import sun.tools.jconsole.ProxyClient.Snapshot

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
        //val resources  = resources0.map(res => bug.cls + re)
        /* at least one of resources is key in map */
        def contains_key(exact: Boolean) = {
          val key = resources.foldLeft[Option[String]](None)((acc, res) => {
            /* if the resources is already a key keep it as a key */
            if (map.contains(res)) Some(res)
            else {
              /* if the resources is part of existing resources for a certain `key` return that `key` */
              acc match {
                case None =>
                  map.foldLeft[Option[String]](acc)((acc0, mapElem) => {
                    val existing_resources = mapElem._2._1
                    val existing_key = mapElem._1
                    if(exact)
                      if (existing_resources.contains(res)) Some(existing_key)
                      else acc0
                    else
                      if (existing_resources.exists(x => res.startsWith(x) && !(x.equals("this")))) Some(existing_key)
                      else acc0
                  })
                case Some(_) => acc
              }
            }
          })
          key
        }

      val key   = contains_key(true)
      val key2 = contains_key(false)

      key match {
        case None =>
          key2 match {
            case None => map.update(resources.head, (resources, List(bug)))
            case Some(key) =>
              val (res, bugs) = map(key)
//              if (bugs.exists(p=> p.equals_weak(bug)))
                map.update(key, ((res ++ resources).distinct, (bugs ++ List(bug)).distinct ))
//              else
//                map.update(resources.head, (resources, List(bug)))
          }
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


  def retrieveSummaryFromSnapshot_opt(snapshot: List[RFSumm]) = {
    val summ = snapshot.foldLeft[Option[RFSumm]](None)((acc,summ) => {
      acc match {
        case None => Some(summ)
        case Some(summ2) => if (lessExpensive(summ.getCost(costSumm), summ2.getCost(costSumm ))) Some(summ) else  acc
      }
    } )
    summ
  }

  def retrieveOneSummaryFromSnapshot(snapshot1: List[RFSumm],snapshot2: List[RFSumm]) = {
    val summ = snapshot1.foldLeft[Option[RFSumm]](None)((acc,summ) => {
     if(Globals.contains_eq(((x:RFSumm,y:RFSumm) => x.procedure == y.procedure),snapshot2,summ)) Some(summ)
     else acc
    } )
    summ match {
      case Some(summ) => List(summ)
      case None => Nil
    }
  }

  def retrieveOneSummary(config: FixConfig, bug:FBug, snapshot1: List[RFSumm], snapshot2: List[RFSumm]): (List[RFSumm],List[RFSumm]) = {
    if(config.atomicity) {
      val summary1 = retrieveSummaryFromSnapshot_opt(snapshot1)
      val summary2 = retrieveSummaryFromSnapshot_opt(snapshot2)

      /* [(Option[Int,AccessKind,String],Boolean)] */
      val line1 = bug.bug_trace.foldLeft[(Option[(Int,org.racerdfix.language.AccessKind,Variable)],Boolean)](None,true)((acc:(Option[(Int,org.racerdfix.language.AccessKind,Variable)],Boolean),te) => {
        if (te.level == 0 && acc._2 && !te.description.contains("Write")) {
          val line   = te.line_number
          val access = {
            summary2 match {
              case None => Write
              case _ => Read
            }
          }
          val resource = RacerDAPI.resourceFromDescription(te.description,bug.cls)
          (Some(line,access,resource), acc._2)
        } else (acc._1, false)
      })

      val line2 = bug.bug_trace.foldLeft[(Option[(Int,org.racerdfix.language.AccessKind,Variable)],Boolean)](None,true)((acc:(Option[(Int,org.racerdfix.language.AccessKind,Variable)],Boolean),te) => {
        val flag = if(te.description.contains("Write")) false else acc._2
        if(te.level == 0 && !flag) {
          val line   = te.line_number
          val access = Write
          val resource = RacerDAPI.resourceFromDescription(te.description,bug.cls)
          (Some(line,access,resource), flag)
        }
        else (acc._1,false)
      })


      val summ1 =
        line1._1 match{
        case None => summary1 match {
          case None => Nil
          case Some(s) => List(s)
        }
        case Some(ln) => {
          val locks = summary1 match {
            case None       => Nil
            case Some(summ) => summ.locks
          }
          val proc = summary1 match {
            case None       => bug.proc
            case Some(summ) => summ.procedure
          }
          List(new RFSumm(bug.file,bug.cls,proc,ln._3,ln._2,locks,ln._1,EmptyTrace,""))
        }
      }

      val summ2 =
        line2._1 match{
          case None => summary2 match {
            case None => Nil
            case Some(s) => List(s)
          }
          case Some(ln) => {
            val locks = summary2 match {
              case None       => Nil
              case Some(summ) => summ.locks
            }
            val proc = summary2 match {
              case None       => bug.proc
              case Some(summ) => summ.procedure
            }
              List(new RFSumm(bug.file,bug.cls,proc,ln._3,ln._2,locks, ln._1,EmptyTrace,""))
          }
        }
      (summ1, summ2)
//
//      val summary1 = retrieveOneSummaryFromSnapshot(snapshot1, snapshot2)
//      val summary2 = retrieveOneSummaryFromSnapshot(snapshot2, summary1)
//      (summary1, summary2)
    } else {
      val summary1 = retrieveSummaryFromSnapshot(snapshot1)
      val summary2 = retrieveSummaryFromSnapshot(snapshot2)
      (summary1, summary2)
    }
  }

  def retrieveSummary(config: FixConfig, bug: FBug) = {
    val summaries = retrieveOneSummary(config,bug, bug.snapshot1,bug.snapshot2)
    summaries._1 ++ summaries._2
  }
}
