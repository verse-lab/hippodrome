import com.facebook.infer.annotation.*;

@ThreadSafe
public class TestUnprotectedW {

}

@ThreadSafe
class A{
    boolean resource = true;

    public void foo(){
        resource = false;
    }
}

/*
What should be the locked object be?
Possibilities:
1. `this` - easiest solution but might lead to over-synchronisation should the code contain other synchronisations on `this` unrelated to `resource`.
2. create a dummy object and sync on that? `Object freshVar = new Object();`

cost function critarias:
1. The more granular the locks the better - for maintainability purposes.
2. The more diverse the locks the better, to avoid over-synchronisation. That is, whenever feasible, create new objects to lock on.
*/