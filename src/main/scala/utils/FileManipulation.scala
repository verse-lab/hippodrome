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
    val _ = copyRenameFile(filename, filename + ".orig.java")
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
}
