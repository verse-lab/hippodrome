import java.util.concurrent.atomic.AtomicBoolean;
import com.facebook.infer.annotation.*;

@ThreadSafe
public class AtomicBooleanBug {
    private AtomicBoolean buggy;

    public AtomicBooleanBug(){
        buggy = new AtomicBoolean();
    }

    public void foo() {
        buggy.set(true);
    }
}