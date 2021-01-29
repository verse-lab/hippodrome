package accountsubtype;

/**
 * This is a completely rewritten version of the IBM "account" example.
 * It displays the same bug, but it makes the bug a bit more subtle by
 * realizing the error in one of two possible sub-types of "Account".
 *
 * @author Matt Dwyer
 */

public class Main {

    /***
     * @params args[0] is the (optional) number of correct accounts to create
     * @params args[1] is the (optional) number of buggy accounts to create
     */
    public static void main(String[] args) {
        int numBusinessAccounts = 20;
        int numPersonalAccounts = 1;

        if (args != null && args.length == 2) {
            numBusinessAccounts = Integer.parseInt(args[0]);
            numPersonalAccounts = Integer.parseInt(args[1]);
        }

        // Create accouns with initial balance of 100
        Bank bank = new Bank(numBusinessAccounts, numPersonalAccounts, 100);
        bank.work();
        bank.printAllAccounts();

        // Check to see that all of the balances are stable
        for (int i = 0; i < numBusinessAccounts + numPersonalAccounts; i++) {
            if (bank.getAccount(i).getBalance() != 300) {
                throw new RuntimeException("bug found");
            }
        }

    }//end of function main
}//end of class Main
