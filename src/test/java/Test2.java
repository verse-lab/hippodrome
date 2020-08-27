 
   
     
       
         
           
             
               
                 
                   
                     
           import com.facebook.infer.annotation.*;           
                                
                                @ThreadSafe                                
                                class Test { Object objR1 =  new Object();            
                                    A myA = new A();                            
                                                                
                                    A myA1 = new A();                                
                      
                      
                                    public void haz (A a) {           
                                        synchronized(objR1) { myA   = a;           
                                        myA.f = 4; }            
                      
                                               
                      
                                    }              
                      
                                   protected void haha(int x){           
                                        synchronized(objR1) { myA.f = x; }            
                                              
                                   }                
                      
                                }                                
                                                                
                                class A { int f = 0;  }