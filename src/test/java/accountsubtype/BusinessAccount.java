package accountsubtype;

public class BusinessAccount extends Account {
  public BusinessAccount(int number, int amnt ) {
    super(number, amnt);
  }

  public synchronized void transfer(Account dest, int transferAmount){
      amount -= transferAmount;
      synchronized (dest) { dest.amount+=transferAmount; }
  }
}
