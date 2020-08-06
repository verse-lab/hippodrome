import org.racerdfix.language.{CSumm, EmptyTrace, Insert, PatchBlock, Read, Write}
import org.racerdfix.TraverseJavaClass.{generateInsertObjectOnCommonResource, generateInsertObjects, generateInsertPatches, generateUpdateObjects, generateUpdatePatches, patchIDGenerator, translateRawSnapshotsToSnapshots}
import org.hamcrest.CoreMatchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test
import org.racerdfix.inferAPI.RacerDAPI


class PatchCreationUnitTest {
    @Test
    @throws[Exception]
    def whenDifferentResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
        val lock1 = RacerDAPI.lockOfString("P<0>{(this:B*).myA2}")
        val lock2 = RacerDAPI.lockOfString("P<0>{(this:B*).myA1}")
        val csumm1 = new CSumm(filename,"B","this->myA->f", Read, List(lock1), 30, EmptyTrace )
        val csumm2 = new CSumm(filename,"B","this->myA", Write, List(lock2), 24, EmptyTrace )

        val (summ1,summ2) = translateRawSnapshotsToSnapshots(csumm1,csumm2)

        /* ************** UPDATES ***************** */
        /* generate update objects */
        val updates1 = generateUpdateObjects(summ1.csumm,summ2.csumm)
        val updates2 = generateUpdateObjects(summ2.csumm,summ1.csumm)

        /* generate update patches */
        val update_patches1:List[(Int, Option[PatchBlock])] = generateUpdatePatches(summ1.csumm.filename, updates1, summ1.tokens, summ1.tree)
        val update_patches2:List[(Int, Option[PatchBlock])] = generateUpdatePatches(summ2.csumm.filename, updates2, summ2.tokens, summ2.tree)

        assertThat(update_patches1.size, is(1))
        assertThat(update_patches2.size, is(1))
        assertThat(update_patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA1){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
        assertThat(update_patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA2){myA = a;})".replaceAll("[\\n\\t ]", ""))))

        /* ************** INSERTS (1)***************** */
        /* generate insert objects */
        val inserts1 = generateInsertObjects(summ1.csumm,summ2.csumm)
        val inserts2 = generateInsertObjects(summ2.csumm,summ1.csumm)

        /* generate inserts patches */
        val insert_patches1 = generateInsertPatches(summ1.csumm.filename, inserts1, summ1.tokens, summ1.tree)
        val insert_patches2 = generateInsertPatches(summ2.csumm.filename, inserts2, summ2.tokens, summ2.tree)


        assertThat(insert_patches1.size, is(1))
        assertThat(insert_patches2.size, is(1))
        assertThat(insert_patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA2){myA = a;})".replaceAll("[\\n\\t ]", ""))))
        assertThat(insert_patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA1){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
    }

    @Test
    @throws[Exception]
    def whenSameResources(): Unit = {
        val filename = "src/test/java/RacyFalseNeg.java"
        val csumm1 = new CSumm(filename, "B","this->myA->f", Read, List(), 30, EmptyTrace )
        val csumm2 = new CSumm(filename,"B","this->myA", Write, List(), 24, EmptyTrace )

        val (summ1,summ2) = translateRawSnapshotsToSnapshots(csumm1,csumm2)

        /* ************** INSERTS (2) ***************** */
        /* generate insert objects */
        val (inserts1:List[Insert], inserts2:List[Insert]) = generateInsertObjectOnCommonResource(csumm1, csumm2)

        /* generate insert patches */
        val patch_id  = patchIDGenerator(0)._2
        val patches1 = generateInsertPatches(summ1.csumm.filename, inserts1, summ1.tokens, summ1.tree, Some(patch_id))
        val patches2 = generateInsertPatches(summ2.csumm.filename, inserts2, summ2.tokens, summ2.tree, Some(patch_id))

        assertThat(patches1.size, is(1))
        assertThat(patches2.size, is(1))
        assertThat(patches1.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA.f){myA.f = x;})".replaceAll("[\\n\\t ]", ""))))
        assertThat(patches2.head._2.toString.replaceAll("[\\n\\t ]", ""),
            is(("Some(synchronized(myA.f){myA = a;})".replaceAll("[\\n\\t ]", ""))))
    }
}
