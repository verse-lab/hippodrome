import org.racerdfix.language.{EmptyTrace, InsertSync, NoFix, Or, PatchBlock, RFSumm, Read, Write}
import org.junit.Test
import org.racerdfix.{ArgParser, FixConfig, Globals, RacerDFix, RacerDFixException, RunConfig}
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.utils.{ASTManipulation, Logging}
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.racerdfix.RacerDFix.{parser, runPatchAndFix}

class PatchCreationUnitTest {

    private val parser = ArgParser.argsParser

    def parseParams(paramString: Array[String], params: FixConfig): FixConfig = {
        val newConfig = RunConfig(params, Globals.def_src_path)
        parser.parse(paramString, newConfig) match {
            case Some(RunConfig(fixConfig, _)) => fixConfig
            case None => throw RacerDFixException("Bad argument format.")
        }
    }

    private def handleInput(args: Array[String]): Unit = {
        val newConfig = RunConfig(FixConfig(), Globals.def_src_path)
        parser.parse(args, newConfig) match {
            case Some(RunConfig(fixConfig, file)) =>
                val config_from_json = fixConfig.config_options.toArray
                val fixConfig_ext = {
                    parser.parse(config_from_json, RunConfig(fixConfig, fixConfig.java_sources_path)) match {
                        case Some(RunConfig(fixConfig, file)) => fixConfig
                        case None => fixConfig
                    }
                }
                Logging.init(fixConfig_ext)
                assertThat(RacerDFix.runPatchAndFix(fixConfig_ext,1),is(0))
                Logging.stop
            case None =>
                System.err.println("Bad argument format.")
        }
    }

    @Test
    @throws[Exception]
    def runAlarmClock(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/alarmclock/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runAirline(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/airline/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runBuggyProgram(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/buggyprogram/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runConsistency(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/consisitency/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runLinkedList(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/linkedlist/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runPingPong(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/pingpong/CONFIG.json" ))
    }


    @Test
    @throws[Exception]
    def runWrongLock2(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/wronglock2/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runWrongLock(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/wrongLock/CONFIG.json" ))
    }



    @Test
    @throws[Exception]
    def runNestedClasses1(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/nestedclasses1/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runNestedClasses2(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/nestedclasses2/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runNestedClasses3(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/nestedclasses3/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runDataRace(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/datarace/CONFIG.json" ))
    }


    @Test
    @throws[Exception]
    def whenDifferentResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
        val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
        val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
//        val csumm1 = new RFSumm(filename, "B", List("this->myA->f"), Read, List(lock1), 30, EmptyTrace, "")
//        val csumm2 = new RFSumm(filename, "B", List("this->myA"), Write, List(lock2), 24, EmptyTrace, "")
//        val ast = new ASTManipulation

//        val (summ1, summ2) = translateRawSnapshotsToSnapshots(csumm1, Some(csumm2), ast)
//
//        summ2 match {
//            case None =>
//            case Some(summ2) => {
//                /* ************** UPDATES ***************** */
//                /* generate update objects */
//                val updates1 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ1.csumm, summ2.csumm))
//                val updates2 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ2.csumm, summ1.csumm))
//
//                /* generate update patches */
//                val update_patches1 = generatePatches(updates1, summ1.ast)
//                val update_patches2 = generatePatches(updates2, summ2.ast)

//                assertThat(update_patches1.._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA1){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
//                assertThat(update_patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA2){myA = a;})".replaceAll("[\\n\\t ]", ""))))
//
//                /* ************** INSERTS (1)***************** */
//                /* generate insert objects */
//                val inserts1 = new Or(NoFix, NoFix).mkOr(generateInsertObjects(summ1.csumm, summ2.csumm))
//                val inserts2 = new Or(NoFix, NoFix).mkOr(generateInsertObjects(summ2.csumm, summ1.csumm))
//
//                /* generate inserts patches */
//                val insert_patches1 = generatePatches( inserts1, summ1.ast)
//                val insert_patches2 = generatePatches( inserts2, summ2.ast)
//
//
//                assertThat(insert_patches1.size, is(1))
//                assertThat(insert_patches2.size, is(1))
//                assertThat(insert_patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA2){myA = a;})".replaceAll("[\\n\\t ]", ""))))
//                assertThat(insert_patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA1){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
//            }
//        }
    }

    @Test
    @throws[Exception]
    def whenSameResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
//        val csumm1 = new RFSumm(filename, "B",List("this->myA->f"), Read, List(), 30, EmptyTrace, "" )
//        val csumm2 = new RFSumm(filename,"B", List("this->myA"), Write, List(), 24, EmptyTrace, "" )
//        val ast = new ASTManipulation

//                assertThat(patches1.size, is(1))
//                assertThat(patches2.size, is(1))
//                assertThat(patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA.f){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
//                assertThat(patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA.f){myA = a;})".replaceAll("[\\n\\t ]", ""))))
//            }
//        }
    }

//    @Test
//    @throws[Exception]
//    def runMainAlgo(): Unit = {
//         val config = RunConfig(FixConfig(), Globals.def_src_path)
//            //Keep teh below
//            val filename = "src/test/java/RacyFalseNeg.java"
//            /* currently they are manually crafted as below */
//            val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
//            val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
//            val csumm1 = new RFSumm(filename,"B","this->myA->f", Read, List(lock1), 30, EmptyTrace, "")
//            val csumm2 = new RFSumm(filename,"B","this->myA", Write, List(lock2), 24, EmptyTrace, "")
//            //
//            //    val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30, EmptyTrace, "")
//            //    val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24, EmptyTrace, "")
//            RacerDFix.(csumm1, csumm2, config)
//
//    }
}
