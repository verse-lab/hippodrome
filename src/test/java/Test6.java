import com.facebook.infer.annotation.*;

class A {
    int f = 0;
}

@ThreadSafe
class Test3 {

    A myA;

    public Test3(A a) { myA = a;}

    public void deposit(int x) {
        myA.f = myA.f + x;
    }

    public void withdraw(int x) {
        myA.f = myA.f - x;
    }

}

@ThreadSafe
class Test5 {

    A a = new A();
    
    public void foo(String[] args) {

        Test3 t = new Test3(a);

        //init
        a.f = 10;

        //operations
        t.deposit(5);
        t.withdraw(2);
    }
}