import org.racerdfix.language.{EmptyTrace, InsertSync, NoFix, Or, PatchBlock, RFSumm, Read, Write}
import org.junit.Test
import org.racerdfix.{ArgParser, FixConfig, Globals, Hippodrome, RacerDFixException, RunConfig}
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.utils.{ASTManipulation, Logging}
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.racerdfix.Hippodrome.{parser, runPatchAndFix}

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
                assertThat(Hippodrome.runPatchAndFix(fixConfig_ext,1),is(0))
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
    def runNIOSImplified(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/nio-io-library-simplified/CONFIG.json" ))
    }

    @Test
    @throws[Exception]
    def runDeadlockPaper(): Unit = {
        handleInput(Array("--testing=true", "--config_file="  + "src/test/resources/java-benchmark/deadlock-paper/CONFIG.json" ))
    }
}
