package accountsubtype;

public abstract class Account {
  protected String  name;
  protected int number;
  protected int amount;

  public Account(int number, int initialBalance) {
	this.name = "Account "+number;
        this.number = number;
        this.amount = initialBalance;
  }

  public synchronized String getName(){
      return name;
  }

  public synchronized int getBalance(){
      return amount;
  }

  public synchronized void deposit(int money){
      amount+=money;
  }

  public synchronized void withdraw(int money){
      amount-=money;
  }

  // Implementing methods should be synchronized
  public abstract void transfer(Account dest, int mn);

  synchronized void print() { 
  }

}
