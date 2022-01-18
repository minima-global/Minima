package org.minima.system.commands.txn.txndb;

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
	
	public ArrayList<TxnRow> mTransactions = new ArrayList<>();
	
	public TxnDB() {}
	
	public void createTransaction(String zKey) {
		mTransactions.add(new TxnRow(zKey, new Transaction()));
	}
	
	public TxnRow getTransactionRow(String zKey) {
		for(TxnRow txn : mTransactions) {
			if(txn.getID().equals(zKey)) {
				return txn;
			}
		}
		return null;
	}
	
	public void deleteTransaction(String zKey) {
		ArrayList<TxnRow> transactions = new ArrayList<>();
		for(TxnRow txn : mTransactions) {
			if(!txn.getID().equals(zKey)) {
				transactions.add(txn);
			}
		}
		mTransactions = transactions;
	}
	
	public ArrayList<TxnRow> listTxns(){
		return mTransactions;
	}
	
	public void clearTxns() {
		mTransactions.clear();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
	}
	
}
