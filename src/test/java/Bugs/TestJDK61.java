 
 import java.lang.ref.ReferenceQueue; 
 import java.lang.ref.WeakReference; 
  
 import com.facebook.infer.annotation.*; 
  
 //import edu.illinois.jacontebe.framework.Reporter; 
  
 /** 
  * Bug URL:https://bugs.openjdk.java.net/browse/JDK-4243978 
  * This is race. 
  * Reproduce environment: JDK 1.6.0_33 
  * 
  * @collector Ziyi Lin 
  **/ 
  
 @ThreadSafe 
 public class TestJDK61 { static Object objR1 =  new Object();  static Object objR8 =  new Object();  
  
     final static boolean debug = false; 
     final static int iterations = 100; 
     final static int gc_trigger = 10; 
     static int[] a = new int[2 * iterations]; 
     // Keep all weak references alive with the following array. 
     static NumberedWeakReference[] b = new NumberedWeakReference[iterations]; 
     static int length; 
  
     public static void main(String[] argv) { 
         // Reporter.reportStart("jdk4243978", 0, "race"); 
         // Get the runtime "object" so that we can force GC. 
         Runtime RunT = Runtime.getRuntime(); 
         // Raise thread priority to match the referenceHandler 
         // priority, so that they can race also on a uniprocessor. 
         raisePriority(); 
         int i; 
         ReferenceQueue Q = new ReferenceQueue(); 
         // Create many reference objects. 
         // Each points to a unique integer object. 
         // Then, get the integers collected, and race with the 
         // collector on queuing (half of) the reference objects. 
         // The weak references are numbered, so we can later 
         // check which of them are queued. 
         Integer Obj = new Integer(0); 
         NumberedWeakReference weaky = new NumberedWeakReference(Obj, Q, 0); 
         for (i = 1; i < iterations; i++) { 
             // Create one object and kill another. 
             Obj = new Integer(i); 
             // Trigger gc each gc_trigger iterations. 
             if ((i % gc_trigger) == 0) 
                 RunT.gc(); 
             // Enqueue the odd objects (from previous iteration). 
             if ((i % 2) == 0) 
                 weaky.enqueue(); 
             // Keep previous weaky alive. 
             synchronized(objR8) { b[i - 1] = weaky; }  
              
             // Get a new weaky for the new object. 
             weaky = new NumberedWeakReference(Obj, Q, i); 
         } 
         // Now do a final collection to round up. 
         RunT.gc(); 
         // Now, after everything has hopefully been queued, let's check 
         // that everything is indeed in the queue. 
         if (debug) 
             System.out.println("Reading the queue"); 
         // Empty queue and record numbers into a[]; 
         i = 0; 
         NumberedWeakReference weakRead = (NumberedWeakReference) Q.poll(); 
         while (weakRead != null) { 
             synchronized(objR1) { a[i++] = weakRead.number; }  
              
             weakRead = (NumberedWeakReference) Q.poll(); 
         } 
         length = i; 
         if (debug) 
             System.out.println("Number of elements in the queue = " + length); 
         // Use the last object or the comipler kills it 
         // Sort the first "length" elements in array "a[]". 
         sort(); 
         // Check results: Are all enqueued? 
         if (debug) 
             System.out.println("Start of final check"); 
         boolean fail = (length != (iterations - 1)); 
         for (i = 0; i < length; i++) { 
             synchronized(objR1) { if (a[i] != i) { 
                 System.out.println("a[" + i + "] is not " + i + " but " + a[i]); 
                 fail = true; 
             } }  
          
         } 
         if (fail) { 
             int shouldBe = iterations - 1; 
             System.out.println("Only " + length 
                     + " reference objects have been queued out of " + shouldBe 
                     + "."); 
             System.out.println(" The following numbers have not been queued: "); 
             int missing = 0; 
             int element = 0; 
             for (i = 0; i < length; i++) { 
                 synchronized(objR1) { while ((a[i] != element) & (element < shouldBe)) { 
                     System.out.print(element + " "); 
                     if (missing % 20 == 19) 
                         System.out.println(" "); 
                     missing++; 
                     element++; 
                 } }  
                  
                 element++; 
             } 
             System.out.print("\n"); 
         } 
         //Reporter.reportEnd(fail); 
     } 
  
     // This bubble sorts the first "length" elements in array "a". 
     public static void sort() { 
         int hold, pass, i; 
         if (debug) 
             System.out.println("Sorting. Length=" + length); 
         for (pass = 1; pass < length; pass++) { // passes over the array 
             for (i = 0; i < length - pass; i++) { // a single pass 
                 synchronized(objR1) { if (a[i] > a[i + 1]) { // then swap 
                     hold = a[i]; 
                     a[i] = a[i + 1]; 
                     a[i + 1] = hold; 
                 } }  
  
              
  
             } // End of i loop 
         } // End of pass loop 
     } 
     // Raise thread priority to compete with the referce handler. 
     // This is (probably) only required for a uniprocessor. 
     static void raisePriority() { 
         Thread tr = Thread.currentThread(); 
         tr.setPriority(Thread.MAX_PRIORITY); 
     } 
 } // ENd of class ref_check 
  
 class NumberedWeakReference extends WeakReference { 
     // Add an integer to identify the weak reference object. 
     int number; 
  
     public NumberedWeakReference(Object referent, ReferenceQueue q, int i) { 
         super(referent, q); 
         number = i; 
     } 
 }