 
  import com.facebook.infer.annotation.*; 
  
  @ThreadSafe 
  class Test {
      Object objR3 =  new Object();
      A myA = new A();
      A myA1 = new A();
  
      public void haz(A a) { 
          a = myA ;
      } 
  
      protected void haha(int x) { 
              myA.f = x;
      }
  } 
  
  class A { 
      int f = 0; 
  }
