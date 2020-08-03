package org.racerdfix

object RacerDAPI {

  /* P<0>{(this:B*).myA2} ==> B */
  def getLock2ClassName_def(lock: String): String = {
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

  def getLock2ClassName(lock: String): String = {
    val result = getLock2ClassName_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> this */
  def getLock2Object_def(lock: String): String = {
    val pattern = "(?<=\\()[^)]+(?=:)".r
    pattern.findFirstMatchIn(lock) match {
      case Some(obj_reg) => obj_reg.toString()
      case None => ""
    }
  }

  def getLock2Object(lock: String): String = {
    val result = getLock2Object_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> myA2*/
  def getLock2Var_def(lock: String): String = {
    val pattern = "(?<=\\).)[^)]+(?=\\})".r
    pattern.findFirstMatchIn(lock) match {
      case Some(resource) => resource.toString()
      case None => getLock2Object(lock)
    }
  }

  def getLock2Var(lock: String): String = {
    val result = getLock2Var_def(lock)
    if (false) {
      println("inp1: " + lock)
      println("out:  " + result)
    }
    result
  }

  /* P<0>{(this:B*).myA2} ==> myA2*/
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
}
