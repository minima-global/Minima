package org.minima.system.commands.txn;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.objects.Transaction;
import org.minima.utils.Streamable;

public class TxnDB implements Streamable {

	private static TxnDB mTxnDB = new TxnDB();
	public static TxnDB getDB() {
		return mTxnDB;
	}
	
	public Hashtable<String, Transaction> mTransactions = new Hashtable<>();
	
	public TxnDB() {}
	
	public void createTransaction(String zKey) {
		mTransactions.put(zKey, new Transaction());
	}
	
	public Transaction getTransaction(String zKey) {
		return mTransactions.get(zKey);
	}
	
	public void deleteTransaction(String zKey) {
		mTransactions.remove(zKey);
	}
	
	public ArrayList<Transaction> list(){
		return new ArrayList<Transaction>( mTransactions.values() );
	}
	
	public void clearTxns() {
		mTransactions.clear();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		// TODO Auto-generated method stub
		
	}
	
}
