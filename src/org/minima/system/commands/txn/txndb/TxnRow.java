package org.minima.system.commands.txn.txndb;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TxnRow implements Streamable {

	public String mID;
	public Transaction 	mTransaction;
	public Witness 		mWitness;
	
	public TxnRow(String zID, Transaction zTransaction, Witness zWitness) {
		mID 			= zID;
		mTransaction 	= zTransaction;	
		mWitness		= zWitness;	
	}
	
	public String getID() {
		return mID;
	}
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public Witness getWitness() {
		return mWitness;
	}
	
	public void clearWitness() {
		mWitness = new Witness();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("id", mID);
		ret.put("transaction", mTransaction.toJSON());
		ret.put("witness", mWitness.toJSON());
		return ret;		
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniString.WriteToStream(zOut, mID);
		mTransaction.writeDataStream(zOut);
		mWitness.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mID = MiniString.ReadFromStream(zIn).toString();
		
		mTransaction = new Transaction();
		mTransaction.readDataStream(zIn);
		
		mWitness = new Witness();
		mWitness.readDataStream(zIn);
	}
}
