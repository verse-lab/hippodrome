package org.racerdfix.inferAPI

import org.racerdfix.language.{EmptyTrace, Lock, NonEmptyTrace, Read, Unk, Variable, Write}

object RacerDAPI {

  /* P<0>{(this:B*).myA2} ==> B */
  def classNameOfLockString_def(lock0: String): String = {
    val lock = lock0.replaceAll("\\$[0-9]","")
    val pattern = "(?<=\\()[^)]+(?=\\))".r
    pattern.findFirstMatchIn(lock) match {
      case Some(cls_reg) =>
        val pat_this = "(?<=([A-Za-z]):)[^)]+".r
        pat_this.findFirstMatchIn(cls_reg.toString()) match {
          case Some(cls_reg_star) => {
            val pat_star =  "[^)]+(?=\\*)".r
            pat_star.findFirstMatchIn(cls_reg_star.toString()) match {
              case Some(cls) => cls.toString()
              case None      => cls_reg_star.toString()
            }
          }
          case None => cls_reg.toString()
        }
      case None => ""
    }
  }

  def classNameOfLockString(lock: String): String = {
    val result = classNameOfLockString_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> this */
  def objectOfLockString_def(lock: String): String = {
    val pattern = "(?<=\\()[^)]+(?=:)".r
    pattern.findFirstMatchIn(lock) match {
      case Some(obj_reg) => obj_reg.toString()
      case None => ""
    }
  }

  def objectOfLockString(lock: String): String = {
    val result = objectOfLockString_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> myA2*/
  def resourceVarOfLockString_def(lock0: String): String = {
    val lock   = lock0.replaceAll("\\$[0-9]","")
    val lock_m = lock.replace("->",".")
    val pattern = "(?<=\\).)[^)]+(?=\\})".r
    pattern.findFirstMatchIn(lock_m) match {
      case Some(resource) => resource.toString()
      case None => objectOfLockString(lock)
    }
  }

  def resourceVarOfLockString(lock: String): String = {
    val result = resourceVarOfLockString_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> (this,B,myA2) */
  def lockOfString_def(str: String): Lock = {
    val cls      = classNameOfLockString_def(str)
    val obj      = objectOfLockString_def(str)
    val resource = resourceVarOfLockString_def(str)
    new Lock(obj, cls, resource)
  }

  def lockOfString(str: String): Lock = {
    val result = lockOfString_def(str)
    if (false) {
      println("inp1: " + str)
      println("out:  " + result.resource)
    }
    result
  }


  def varOfResource_def(resource0: String, cls: String): Variable = {
    val cls_lst = cls.split("\\$")
    val resource = resource0.replaceAll("\\$[0-9]","");;
    /* this->myA2 ==> myA2*/
    val pattern1 = "(?<=->)[^)]+".r
    def replace_arrow(str: String) = str.replace("->",".")
    val resource1 = pattern1.findFirstMatchIn(resource) match {
      case Some(res) => List((res.toString()), (resource))
      case None => List((resource))
    }
    /* "*(buggyprogram.BuggyProgram.history)[_]" => history */
    val resource2 = try {
      val pattern2 = "(?<=\\*\\()[^)\\[]+".r
      val resource_lst = resource1.foldLeft[List[String]](Nil)((acc,res) => {
        pattern2.findFirstMatchIn(res) match {
          case Some(resource) => (classToListOfCls(resource.toString()).head) :: acc
          case None => res ::acc
        }
      })
      resource_lst
    } catch  {
      case  _ => resource1
    }
    /* "*iDest1[_]" => iDest1 */
    val resource3 = try {
      val pattern3 = "(?<=\\*)[^)\\[]+".r
      val resource_lst = resource2.foldLeft[List[String]](Nil)((acc,res) => {
        pattern3.findFirstMatchIn(res) match {
          case Some(resource) => (classToListOfCls(resource.toString()).head) :: acc
          case None => res ::acc
        }
      })
      resource_lst
    } catch  {
      case  _ => resource2
    }

    /* buggyprogram.BuggyProgram.history => history */
    val resource4 = try {
      resource3.foldLeft[List[String]](Nil)((acc,res) => {
        classToListOfCls(res.toString()).head :: acc
      })
    } catch {
      case _ => resource3
    }
    /* for cls.cls$X get the X'th element from cls_lst */
    val aliases = resource4.distinct.map(replace_arrow)
    val id      =  aliases.foldLeft(resource0)((acc,v) => if (v.length < acc.length) v else acc )
    val cls0    =
      try {
        val pattern = "((?<=\\$)[0-9]+)".r
        val possible_idx = pattern.findAllIn(resource0).toList
        val idx = possible_idx(possible_idx.length - 1).toInt
        cls_lst(idx)
      } catch {
        case _ => cls
      }
    new Variable( cls = cls0 , id = id, aliases= aliases)
  }

  def varOfResource(resource: String, cls: String = ""): Variable = {
    val result = varOfResource_def(resource,cls)
    if (false) {
      println("inp1: " + resource)
      println("out:  " + result)
    }
    result
  }

  /* this->myA2 ==> myA2*/
//  def getResource2Var_def(resource: String): String = {
//    val pattern = "(?<=->)[^)]+".r
//    pattern.findFirstMatchIn(resource) match {
//      case Some(resource) => resource.toString().replace("->",".")
//      case None => resource
//    }
//  }
//
//  def getResource2Var(resource: String): String = {
//    val result = getResource2Var_def(resource)
//    if (false) {
//      println("inp1: " + resource)
//      println("out:  " + result)
//    }
//    result
//  }

  /* "B.<init>()" => "B" */
  def classNameOfMethodString_def(method: String): String ={
    val pattern = "[^)]+(?=\\()".r
    pattern.findFirstMatchIn(method) match {
      case Some(resource) => {
        val pattern = "[^)]+(?=\\.)".r
        pattern.findFirstMatchIn(resource.toString()) match {
          case Some(cls) => cls.toString
          case None      => resource.toString()
        }
      }
      case None => method
    }
  }

  def classNameOfMethodString(method: String): String = {
    val result = classNameOfMethodString_def(method)
    if (false) {
      println("inp1: " + method)
      println("out:  " + result)
    }
    result
  }

  def accessKindOfString(str: String) = {
    if (str == "Read") Read
    else if (str == "Write") Write
    else Unk
  }

  def traceOfListOfStrings(trace: List[String]) = {
    trace match {
      case Nil => EmptyTrace
      case _  => new NonEmptyTrace(trace)
    }
  }

  /* "A.B.C" => ["A","A.B","A.B.C"] */
  def refToListOfRef(sp0: String) = {
    val sp    = sp0.replaceAll("\\$[0-9]","")
    val lst   = sp.split(Array('.')).toList
    val vars  = lst.foldLeft((Nil:List[String],""))((acc:(List[String],String),str) => {
      val vr = acc._2 match {
        case "" => str
        case _  => acc._2 + "." + str
      }
      (acc._1 ++ List(vr), vr)})
    vars._1
  }

  /* "A.B.C" => ["C","B.C","A.B.C"] */
  def classToListOfCls(sp0: String) = {
    val sp1   = sp0.replaceAll("\\$[0-9]","")
    val sp    = sp1.replaceAll("\\$",".")
    val lst   = sp.split(Array('.')).toList
    val vars  = lst.foldRight((Nil:List[String],""))((str,acc:(List[String],String)) => {
      val vr = acc._2 match {
        case "" => str
        case _  => str + "." + acc._2
      }
      (acc._1 ++ List(vr), vr)})
    vars._1
  }
}
