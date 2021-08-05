package datarace;
 import com.facebook.infer.annotation.*; 
  
  
 @ThreadSafe 
 public class CustomerInfo { Object objR1 =  new Object();  
  
 	private int nAccount; 
 	private Account[] accounts; 
 	 
 	public CustomerInfo() { 
 		super(); 
 	} 
  
 	public CustomerInfo(int nAccount, Account[] accounts) { 
 		super(); 
 		this.nAccount = nAccount; 
 		this.accounts = accounts; 
 	} 
  
 	public void withdraw(int accountNumber, int amount){
 		Object o = new Object();
 		synchronized (o) {
			int temp = accounts[accountNumber].getBalance();
			temp = temp - amount;
			accounts[accountNumber].setBalance(temp);
			System.out.println("withdraw " + amount + "now " + accounts[accountNumber].getBalance());
			int x = 0;
		}
 	} 

 	public void deposit(int accountNumber, int amount) {
		Object o = new Object();
		synchronized (o) {
		int temp = accounts[accountNumber].getBalance();
		temp = temp + amount;
		accounts[accountNumber].setBalance(temp);
		System.out.println("deposit " + amount + "now " + accounts[accountNumber].getBalance());
		int x = 5;
	}
 	} 
 	/*
 	public boolean check(int accountNumber, int amount) { 
 		return accounts[accountNumber].getBalance() == amount; 
 	}
 	*/
 }