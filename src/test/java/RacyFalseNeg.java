import com.facebook.infer.annotation.*;
                    
          // @ThreadSafe          
          public class RacyFalseNeg {          
                    
              public void foo(B b) {          
                  final A a = new A();   
                  final A c = new A();   
                  synchronized (this) {      
                      b.haz(a);          
                      b.haha(42);
                  }          
              }          
          }          
                    
          @ThreadSafe          
          class B {   
              A myA = new A();      
                    
              A myA1 = new A();          
              A myA2 = new A();    
              A myA3 = new A();    
        
              public void haz (A a) {          
                 synchronized(myA1) {
                      myA = a;    
                 }
              }      
                    
             protected void haha(int x){
                 //synchronized(myA2){
                     myA.f = x;
                 //}
             }    
      

            /*protected void foo(A x){
                  synchronized(myA3){
                      myA = x;   
                  }
              }*/
          }          
                    
          class A { int f = 0; }      
            
                    
          /**          
                    
                    
           ==== B.haz(A)          
                    
           Accesses {          
           {elem= Access: Write to this->myA Thread: AnyThread Lock: false Pre: OwnedIf{ 0 }; loc= line 18; trace= { }} }          
                    
           PRE:          
           a = val$3:; this = val$1: ;          
           this|->{myA:val$2}:          
           POST 1 of 1:          
           a = val$3:; return = val$4:; this = val$1: ;          
           this|->{myA:a}:          
                    
           ==== B.haha(int)          
                    
           Accesses {          
           {elem= Access: Read of this->myA Thread: AnyThread Lock: false Pre: OwnedIf{ 0 }; loc= line 22; trace= { }},          
           {elem= Access: Write to this->myA->f Thread: AnyThread Lock: false Pre: OwnedIf{ 0 }; loc= line 22; trace= { }} }          
                    
           PRE:          
           b = val$2:; this = val$1: ;          
           b|->{myA:val$3}:; this|->{}:          
           POST 1 of 1:          
           MEMne<new:6>(val$5) ;          
           b = val$2:; return = val$4:; this = val$1: ;          
           val$5|->{f:42}:; b|->{myA:val$5}:; this|->{}:          
                    
                    
           ==== Racy.foo(B)          
                    
           Accesses {          
           {elem= Access: Read of b->myA Thread: AnyThread Lock: true Pre: OwnedIf{ 1 }; loc= line 22; trace= { void B.haha(int) at line 9 }},          
           {elem= Access: Write to b->myA Thread: AnyThread Lock: true Pre: OwnedIf{ 1 }; loc= line 18; trace= { void B.haz(A) at line 8 }},          
           {elem= Access: Write to b->myA->f Thread: AnyThread Lock: true Pre: OwnedIf{ 1 }; loc= line 22; trace= { void B.haha(int) at line 9 }} }           
                    
                    
           PRE:          
           b = val$2:; this = val$1: ;          
           b|->{myA:val$3}:; this|->{}:          
           POST 1 of 1:          
           MEMne<new:6>(val$5) ;          
           b = val$2:; return = val$4:; this = val$1: ;          
           val$5|->{f:42}:; b|->{myA:val$5}:; this|->{}:          
                    
                    
           */