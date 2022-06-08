package org.minima.database.userprefs.txndb;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class TxnDB implements Streamable {

	public ArrayList<TxnRow> mTransactions = new ArrayList<>();
	
	public TxnDB() {}
	
	public void loadDB() {
		MiniData completedb = MinimaDB.getDB().getUserDB().loadCustomTransactions();
		if(!completedb.isEqual(MiniData.ZERO_TXPOWID)) {
			TxnDB txnDB 	= TxnDB.convertMiniDataVersion(completedb);
			mTransactions 	= txnDB.mTransactions; 
		}
	}
	
	public void saveDB() {
		MinimaDB.getDB().getUserDB().saveCustomTransactions(MiniData.getMiniDataVersion(this));
	}
	
	public void createTransaction(String zKey) throws Exception {
		mTransactions.add(new TxnRow(zKey, new Transaction(), new Witness()));
	}
	
	public void addCompleteTransaction(TxnRow zRow) {
		//Remove the old
		deleteTransaction(zRow.getID());
		
		//Add the new
		mTransactions.add(zRow);
	}
	
	public TxnRow getTransactionRow(String zKey) {
		for(TxnRow txn : mTransactions) {
			if(txn.getID().equals(zKey)) {
				return txn;
			}
		}
		return null;
	}
	
	public boolean deleteTransaction(String zKey) {
		boolean found = false;
		ArrayList<TxnRow> transactions = new ArrayList<>();
		for(TxnRow txn : mTransactions) {
			if(!txn.getID().equals(zKey)) {
				transactions.add(txn);
			}else {
				found = true;
			}
		}
		mTransactions = transactions;
		
		return found;
	}
	
	public ArrayList<TxnRow> listTxns(){
		return mTransactions;
	}
	
	public void clearTxns() {
		mTransactions.clear();
	}

	/**
	 * Convert a MiniData version into a TxnRow
	 */
	public static TxnDB convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		TxnDB txnrow = null;
		
		try {
			//Convert data
			txnrow = TxnDB.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return txnrow;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//How many
		MiniNumber.WriteToStream(zOut, mTransactions.size());
		for(TxnRow txnrow : mTransactions) {
			txnrow.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTransactions = new ArrayList<>();
		int num = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<num;i++) {
			mTransactions.add(TxnRow.ReadFromStream(zIn));
		}
	}
	
	public static TxnDB ReadFromStream(DataInputStream zIn) throws IOException {
		TxnDB txp = new TxnDB();
		txp.readDataStream(zIn);
		return txp;
	}
	
}
