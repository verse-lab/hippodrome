package org.racerdfix.utils

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import scala.io.Source

class FileManipulation {

  def copyRenameFile(source: String, destination: String) = {
    val path = Files.copy(
      Paths.get(source),
      Paths.get(destination),
      StandardCopyOption.REPLACE_EXISTING
    )
    path.toString
  }

  def cloneOriginalFile(filename: String): Unit = {
    val _ = copyRenameFile(filename, filename + ".orig")
  }

  def revertToOriginalFile(filename: String): Unit = {
    val _ = copyRenameFile(filename + ".orig", filename)
  }

  def cloneOriginalFileToFix(filename: String) = {
    val path = copyRenameFile(filename, filename + ".fix.java")
    println("path: " + path)
    path
  }

  def overwriteFile(filename: String, text: String): Unit ={
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def fileToString(filename: String) = {
    Source.fromFile(filename).getLines.foldLeft("") { (str, line) => str + " \n " + line.toString }
  }

  def getFile(path: String, filename: String) = {
     /* TODO strip the last / from path*/
     val path_processes = if(path.length > 0 && path.last == '/') path.dropRight(1) else path
     path_processes + "/" + filename
  }

  def getPath(path1: String, path2: String) = {
    /* TODO strip the last / from path*/
    val path1_processes = if(path1.length > 0 && path1.last == '/') path1.dropRight(1) else path1
    val path2_processes = if(path2.length > 0 && path2.last == '/') path2.dropRight(1) else path2
    path1_processes + "/" + path2_processes
  }

  def getListOfFiles(dir: String): List[String] = {
    try {
      val file = new File(dir)
      file.listFiles.filter(_.isFile)
        .map(_.getPath).toList
    } catch {
      case _ => {
        println( "Check that your " + dir + " is valid!" )
        Nil
      }
    }
  }

}
