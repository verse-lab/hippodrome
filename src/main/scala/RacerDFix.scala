package org.racerdfix

import org.racerdfix.TraverseJavaClass.mainAlgo
import org.racerdfix.fixdsl.{CSumm, Read, Write}

object RacerDFix {

  def runPatchAndFix() = {
    val filename = "src/test/java/RacyFalseNeg.java"
    /* retrieve summary bug (e.g. two conflicting summaries) */
    /* TODO: read the summary bugs from a JSON file */
    /* currently they are manually crafted as below */
    /* {elem= Access: Read of this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA2} } Pre: OwnedIf{ 0 }; loc= line 30; trace= { }},*/
    /* {elem= Access: Write to this->myA Thread: AnyThread Lock: true Acquisitions: { P<0>{(this:B*)->myA1} } Pre: OwnedIf{ 0 }; loc= line 24; trace= { }} },*/
    val csumm1 = new CSumm(filename,"B","this->myA->f", Read, List("P<0>{(this:B*).myA2}"), 30 )
    val csumm2 = new CSumm(filename,"B","this->myA", Write, List("P<0>{(this:B*).myA1}"), 24 )
    //
    //    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30 )
    //    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24 )
    mainAlgo(csumm1, csumm2)

  }

  def main(args: Array[String]) = runPatchAndFix()
}
