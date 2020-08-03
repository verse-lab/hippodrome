package com.racerdfix

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

class FileManipulation {

  def copyRenameFile(source: String, destination: String): Unit = {
    val path = Files.copy(
      Paths.get(source),
      Paths.get(destination),
      StandardCopyOption.REPLACE_EXISTING
    )
    // could return `path`
  }

  def cloneOriginalFile(filename: String): Unit = {
    copyRenameFile(filename, filename + ".orig.java")
  }

  def overwriteOriginalFile(filename: String, text: String): Unit ={
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }
}
