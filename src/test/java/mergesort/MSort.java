package mergesort;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import com.facebook.infer.annotation.*;

/**
 * <p>Title: <MultiThreading Merge Sort/p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 *
 * @author unascribed
 * @version 1.0
 * <p>
 * Summary description for MainFRM.
 * <p>
 * Summary description for MainFRM.
 */

/**
 * Summary description for MainFRM.
 */


/**
 * Summary description for Class1.
 */
@ThreadSafe
public class MSort extends Thread {
    private static String outputFile;
    private static BufferedWriter bWriter;
    private static FileWriter fWriter;

    //Increase available threads number by 1,usually done after child thread died
    public synchronized void IncreaseThreadCounter() {
        m_iCurrentThreadsAlive--;

    }

    //Decrease available threads number by 1,usually done before runnning a child thread
    public synchronized void DecreaseThreadCounter()//Decrease
    {

        m_iCurrentThreadsAlive++;

    }

    //Get a state of available threads in the system and allocate them
    public static synchronized int AvailableThreadsState()//0 - No threads,1 - 1 thread available,2 - 2 threads avilable
    {
        return 1;
    }

    //Reset currently available threads number to 1,should be done in main,before starting of sorting
    public void ResetThreadCounter() {
        m_iCurrentThreadsAlive = 1;
    }

    //Constractor-get's the vector
    public MSort(int[] iArray) {


    }

    //Constractor-get's the command line string
    public MSort(String[] sCommandLine, String fileName) {
    }

    //Constractor - silence
    public MSort() {
        bIsInit = false;
    }

    //Print sorted vector - invoked from main()
    public void PrintResults() {
    try {
        fWriter = new FileWriter("sortedoutput.txt");
        bWriter = new BufferedWriter(fWriter);

        bWriter.write("Sorted using " + m_iThreadLimit + " thread/s");
        bWriter.newLine();
        for (int iCnt = 0; iCnt < m_iArray.length; iCnt++) {
            bWriter.write(iCnt + " : " + m_iArray[iCnt]);
            bWriter.newLine();
        }
        bWriter.close();
    } catch(IOException e){

        }
    }


    //Internal MergeSort procedure
    public void CopyArrays(int[] iSource, int[] iDest1, int[] iDest2) {
        for (int iCnt = 0; iCnt < iDest1.length; iCnt++)
            iDest1[iCnt] = iSource[iCnt];
        for (int iCnt = 0; iCnt < iDest2.length; iCnt++)
            iDest2[iCnt] = iSource[iCnt + iDest1.length];
    }

    public void Sorting() {
        int iSize = m_iArray.length;
        int[] iA = new int[iSize / 2];//Split a source array to Left/Right arrays
        int[] iB = new int[iSize - (iSize / 2)];
        CopyArrays(m_iArray, iA, iB);
        Merge(iA, iB, m_iArray);
    }

    //An internal recursive sort procedure
    public void Merge(int[] iA, int[] iB, int[] iVector) {
        int iALength = iA.length;
        int iBLength = iB.length;

        if ((iALength + iBLength) == 1) {
            if (iALength == 1)
                iVector[0] = iA[0];
            else
                iVector[0] = iB[0];

            return;
        }
        
        return;
    }

    private int[] m_iArray; //Vector to be sorted
    private static int m_iThreadLimit;//Tread limit - have to be static
    private static int m_iCurrentThreadsAlive;//Thread Counter - have to be static
    private boolean bIsInit;//If in false state - the sorting wouldn't perform
}
