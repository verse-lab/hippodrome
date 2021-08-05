import com.facebook.infer.annotation.*;

@ThreadSafe
public class LocalThreads {

    private String sharedString = null;

    public LocalThreads() {
        HelloHelper h = new HelloHelper();

        Thread thread1 = new Thread1();
        Thread thread2 = new Thread2();

        thread1.start();
        thread2.start();
    }

    public static void main(String args[]) {
        new LocalThreads();
    }

    private class Thread1 extends Thread {
            public void run() {
                sharedString = sharedString + "from T1";
                System.out.println(sharedString);
            }
        }

        private class Thread2 extends Thread {
            public void run() {
                sharedString = sharedString + "from T2";
                System.out.println(sharedString);
            }
        }
}