package org.racerdfix.utils

import java.io.{BufferedWriter, FileWriter}

import org.racerdfix.{FixConfig, Globals}
import org.racerdfix.RacerDFix.RunConfig

object Logging {
   var config = RunConfig(FixConfig(), Globals.def_src_path).fixConfig
   var bw: BufferedWriter = _

   def init(fixConfig: FixConfig) = {
     config = fixConfig
     open
   }

   def stop = {
      close
   }

   def open = {
     if(config.log) {
       bw = new BufferedWriter(new FileWriter(config.log_file))
     }
   }

  def close = {
    if(config.log) {
      bw.close()
    }
  }

   def add(str: String) = {
     if(config.log) {
       bw.write(str)
     }
   }
}
