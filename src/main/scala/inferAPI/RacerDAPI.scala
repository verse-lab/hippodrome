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

  /* P<0>{(this:B*).myA2} ==> B */
  def lockOfString(str: String): Lock = {
    val cls      = classNameOfLockString_def(str)
    val obj      = objectOfLockString_def(str)
    val resource = resourceVarOfLockString_def(str)
    new Lock(obj, cls, resource)
  }


  /* this->myA2 ==> myA2*/
  def getResource2Var_def(resource: String): String = {
    val pattern = "(?<=->)[^)]+".r
    pattern.findFirstMatchIn(resource) match {
      case Some(resource) => resource.toString().replace("->",".")
      case None => resource
    }
  }

  def getResource2Var(resource: String): String = {
    val result = getResource2Var_def(resource)
    if (false) {
      println("inp1: " + resource)
      println("out:  " + result)
    }
    result
  }

  /* "B.<init>()" */
  def classNameOfMethodString_def(method: String): String ={
    val pattern = "[^)]+(?=\\.)".r
    pattern.findFirstMatchIn(method) match {
      case Some(resource) => resource.toString()
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
    else throw new Exception("BugShort expected")
  }

  def traceOfListOfStrings(trace: List[String]) = {
    trace match {
      case Nil => EmptyTrace
      case _  => new NonEmptyTrace(trace)
    }
  }
}
