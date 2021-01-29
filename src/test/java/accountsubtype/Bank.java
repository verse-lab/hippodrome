package accountsubtype;

public class Bank {
  Manager[] managers;
  Account[] accounts;

  public Bank(int numBusinessAccounts, int numPersonalAccounts, int initialBalance) {
    accounts = new Account[numBusinessAccounts+numPersonalAccounts];
    int i;
    for (i=0; i<numBusinessAccounts; i++) {
      accounts[i] = new BusinessAccount(i, initialBalance);
    }
    for (; i<numBusinessAccounts+numPersonalAccounts; i++) {
      accounts[i] = new PersonalAccount(i, initialBalance);
    }

    managers = new Manager[numBusinessAccounts+numPersonalAccounts];
    for (i=0; i<managers.length; i++) {
      managers[i] = new Manager(this, accounts[i], i);
    }
  }

  /**
   * Start all of the account managers working and wait for their completion.
   */
  public void work() {
    for (int i=0; i<managers.length; i++){
      managers[i].start();
    }

    try {
      for (int i=0; i<managers.length; i++){
        managers[i].join();
      }
    } catch (InterruptedException ie) {}
  }

  public Account getAccount(int number) {
    return accounts[number];
  }

  public int nextAccountNumber(int number) {
    return (number + 1) % accounts.length;
  }

  public void printAllAccounts(){
    for (int i=0; i<accounts.length; i++){
      if (accounts[i]!=null) {
        accounts[i].print();
      }
    }
  }

}
