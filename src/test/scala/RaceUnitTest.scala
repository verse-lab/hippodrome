import org.racerdfix.language.{EmptyTrace, InsertSync, NoFix, Or, PatchBlock, RFSumm, Read, Write}
import org.junit.Test
import org.racerdfix.{ArgParser, FixConfig, Globals, Hippodrome, RacerDFixException, RunConfig}
import org.racerdfix.inferAPI.{InterpretJson, RacerDAPI}
import org.racerdfix.utils.{ASTManipulation, Logging}
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.racerdfix.Hippodrome.{detectAndFix, parser}

class PatchCreationUnitTest {

    private val parser = ArgParser.argsParser

    /* This method must stay in sync with its corresponding in Hippodrome.scala */
    private def mergeConfigurations(config: FixConfig) = {
        val jsonTranslator = new InterpretJson(config)
        val toolConfig = jsonTranslator.getJsonToolConfig()
        /** the `infer` in file-config overwrites the `infer` in the tool-config which overwrites the `Globals.def_infer` */
        val iConfig0 = if (config.infer == "")  config.copy(infer = toolConfig.infer) else config
        /** the `json_path` in file-config overwrites the `json_path` in the tool-config which overwrites the `Globals.results_out_dir` */
        val iConfig = if (iConfig0.json_path == "")  iConfig0.copy(json_path = toolConfig.json_path) else iConfig0
        val inferOptions = toolConfig.infer_opt diff iConfig.infer_opt
        if (inferOptions.isEmpty) iConfig else iConfig.copy(infer_opt = iConfig.infer_opt.concat(inferOptions))
    }

    /* This method must stay in sync with its corresponding in Hippodrome.scala */
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
                val config = mergeConfigurations(fixConfig_ext)
                Logging.init(config)
                assertThat(Hippodrome.detectAndFix(config,1),is(0))
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
