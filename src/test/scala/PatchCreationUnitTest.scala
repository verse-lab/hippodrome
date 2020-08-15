import org.racerdfix.language.{EmptyTrace, InsertSync, NoFix, Or, PatchBlock, RFSumm, Read, Write}
import org.racerdfix.TraverseJavaClass.{generateInsertObjectOnCommonResource, generateInsertObjects, generatePatches, generateUpdateObjects, translateRawSnapshotsToSnapshots}
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test
import org.racerdfix.{FixConfig, Globals, RacerDFix}
import org.racerdfix.RacerDFix.RunConfig
import org.racerdfix.inferAPI.RacerDAPI
import org.racerdfix.utils.ASTManipulation


class PatchCreationUnitTest {
    @Test
    @throws[Exception]
    def whenDifferentResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
        val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
        val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
        val csumm1 = new RFSumm(filename, "B", "this->myA->f", Read, List(lock1), 30, EmptyTrace, "")
        val csumm2 = new RFSumm(filename, "B", "this->myA", Write, List(lock2), 24, EmptyTrace, "")
        val ast = new ASTManipulation

        val (summ1, summ2) = translateRawSnapshotsToSnapshots(csumm1, Some(csumm2), ast)

        summ2 match {
            case None =>
            case Some(summ2) => {
                /* ************** UPDATES ***************** */
                /* generate update objects */
                val updates1 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ1.csumm, summ2.csumm))
                val updates2 = new Or(NoFix, NoFix).mkOr(generateUpdateObjects(summ2.csumm, summ1.csumm))

                /* generate update patches */
                val update_patches1 = generatePatches(updates1, summ1.ast)
                val update_patches2 = generatePatches(updates2, summ2.ast)

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
            }
        }
    }

    @Test
    @throws[Exception]
    def whenSameResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
        val csumm1 = new RFSumm(filename, "B","this->myA->f", Read, List(), 30, EmptyTrace, "" )
        val csumm2 = new RFSumm(filename,"B","this->myA", Write, List(), 24, EmptyTrace, "" )
        val ast = new ASTManipulation
        val (summ1,summ2) = translateRawSnapshotsToSnapshots(csumm1,Some(csumm2),ast)

        summ2 match {
            case None =>
            case Some(summ2) => {
                /* ************** INSERTS (2) ***************** */
                /* generate insert objects */
                val (inserts1: List[InsertSync], inserts2: List[InsertSync]) = generateInsertObjectOnCommonResource(csumm1, csumm2)
                val ins1 = new Or(NoFix, NoFix).mkOr(inserts1)
                val ins2 = new Or(NoFix, NoFix).mkOr(inserts2)

                /* generate insert patches */
                val patch_id = RacerDFix.patchIDGeneratorRange(0)._2
                val patches1 = generatePatches(ins1, summ1.ast, Some(patch_id))
                val patches2 = generatePatches(ins2, summ2.ast, Some(patch_id))
//
//                assertThat(patches1.size, is(1))
//                assertThat(patches2.size, is(1))
//                assertThat(patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA.f){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
//                assertThat(patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
//                    is(("Some(synchronized(myA.f){myA = a;})".replaceAll("[\\n\\t ]", ""))))
            }
        }
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
