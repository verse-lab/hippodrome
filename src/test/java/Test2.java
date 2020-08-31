 
   
     
       
         
           
             
               
                 
                   
          import com.facebook.infer.annotation.*;          
                              
                               @ThreadSafe                               
                               class Test {          
                                   A myA = new A();                           
                                                              
                                   A myA1 = new A();                               
                    
                    
                                   public void haz (A a) {          
                                       myA   = a;          
                                       myA.f = 4;          
                    
                                   }             
                    
                                  protected void haha(int x){          
                                       myA.f = x;          
                                  }               
                    
                               }                               
                                                              
                               class A { int f = 0;  }