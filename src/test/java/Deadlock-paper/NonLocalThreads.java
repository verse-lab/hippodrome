import com.facebook.infer.annotation.*;

@ThreadSafe
  class HelloHelper {  
    Object l = new Object();  
    Object p = new Object();  
    Object GlobalLock = new Object();  
    
    private String sharedString = null;   

    public void t1() {
          sharedString = sharedString + "from T1";
          System.out.println(sharedString);
    }
    
    
    public void t2() {
          sharedString = sharedString + "from T2";
          System.out.println(sharedString);
    }
  }   
    
  public class LocalThreads { 
     public static void main(String[] args){  
        HelloHelper h = new HelloHelper();  
    
        Thread thread1 = new Thread(){ public void run() { h.t1(); } };  
        Thread thread2 = new Thread(){ public void run() { h.t2(); } }; 
    
        thread1.start();  
        thread2.start(); 
     }  
  }