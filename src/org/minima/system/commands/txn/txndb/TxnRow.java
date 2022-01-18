package org.minima.system.commands.txn.txndb;

import org.minima.objects.Transaction;
import org.minima.utils.json.JSONObject;

public class TxnRow {

	public String mID;
	public Transaction mTransaction;
	
	public TxnRow(String zID, Transaction zTransaction) {
		mID 			= zID;
		mTransaction 	= zTransaction;	
	}
	
	public String getID() {
		return mID;
	}
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("id", mID);
		ret.put("transaction", mTransaction.toJSON());
		return ret;		
	}
}
