package accountsubtype;

public class PersonalAccount extends Account {
  public PersonalAccount(int number, int initialBalance) {
    super(number, initialBalance);
  }

  public synchronized void transfer(Account ac, int mn){
      amount-=mn;
      // BUG : update to field of "non-this" account is unprotected
      ac.amount+=mn;
  }
}
