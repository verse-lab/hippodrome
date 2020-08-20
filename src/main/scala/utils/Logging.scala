package org.racerdfix.utils

import java.io.{BufferedWriter, FileWriter}

import org.racerdfix.{FixConfig, Globals, RunConfig}

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
       bw.write(str + "\n")
     }
   }

   def addTime[A,B](str: String, fnc: A => B, arg: A): B = {
     val time_start = System.currentTimeMillis()
     val res = fnc(arg)
     val time_stop  = System.currentTimeMillis()
     bw.write(str + (time_stop - time_start) + " ms "  + "\n")
     res
   }
}
