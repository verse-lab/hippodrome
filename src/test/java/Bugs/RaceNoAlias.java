 

     /** The fix adds weird cross-method scoping. */

       import com.facebook.infer.annotation.*;

                 @ThreadSafe
                 class B {
                     A myA = new A();

                     public A getMyA() {
                         return myA;
                     }

                     public void haz (A a) {
                          a = myA;
                     }

                     protected void haha(Size x){
                         myA = getMyA();
                     }
                 }

                 class A {
                     int f = 0;
                     Size size;
                 }

                 class Size{
                     int val;
                 }


