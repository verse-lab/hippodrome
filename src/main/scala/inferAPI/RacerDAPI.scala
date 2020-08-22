package org.racerdfix.inferAPI

import org.racerdfix.language.{EmptyTrace, Lock, NonEmptyTrace, Read, Write}

object RacerDAPI {

  /* P<0>{(this:B*).myA2} ==> B */
  def classNameOfLockString_def(lock: String): String = {
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
  def resourceVarOfLockString_def(lock: String): String = {
    val pattern = "(?<=\\).)[^)]+(?=\\})".r
    pattern.findFirstMatchIn(lock) match {
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

  /* this->myA2 ==> myA2*/
  def varOfResource_def(resource: String): List[String] = {
    val pattern1 = "(?<=->)[^)]+".r
    def replace_arrow(str: String) = str.replace("->",".")
    val resource1 = pattern1.findFirstMatchIn(resource) match {
      case Some(res) => List(replace_arrow(res.toString()), replace_arrow(resource))
      case None => List(replace_arrow(resource))
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
    resource3
  }

  def varOfResource(resource: String): List[String] = {
    val result = varOfResource_def(resource)
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
    println("ACCESS KIND: " + str)
    if (str == "Read") Read
    else if (str == "Write") Write
    else throw new Exception("BugShort expected")
  }

  def traceOfListOfStrings(trace: List[String]) = {
    trace match {
      case Nil => EmptyTrace
      case _  => new NonEmptyTrace(trace)
    }
  }

  /* "A.B.C" => ["A","A.B","A.B.C"] */
  def refToListOfRef(sp: String) = {
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
  def classToListOfCls(sp: String) = {
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
