package org.minima.database.userdb.java;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.userdb.UserDBRow;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class JavaUserDBRow implements UserDBRow, Streamable {

	int mID;
	
	Transaction mTransaction;
	
	Witness mWitness;
	
	public JavaUserDBRow() {}
	
	public JavaUserDBRow(int zID) {
		mID = zID;
		mTransaction = new Transaction();
		mWitness = new Witness();
	}

	@Override
	public int getID() {
		return mID;
	}

	@Override
	public Witness getWitness() {
		return mWitness;
	}

	@Override
	public Transaction getTransaction() {
		return mTransaction;
	}

	@Override
	public void setTransaction(Transaction zTrans) {
		mTransaction = zTrans;
	}

	@Override
	public void setWitness(Witness zWitness) {
		mWitness = zWitness;
	}
	
	@Override
	public String toString() {
		return "ID:"+mID+" Witness:"+mWitness+" Txn:"+mTransaction;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeInt(mID);
		mTransaction.writeDataStream(zOut);
		mWitness.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mID = zIn.readInt();
		
		mTransaction = new Transaction();
		mTransaction.readDataStream(zIn);
		
		mWitness = new Witness();
		mWitness.readDataStream(zIn);
	}

	@Override
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("id",mID);
		obj.put("transaction",mTransaction.toJSON());
		obj.put("witness",mWitness.toJSON());
		
		return obj;
	}
}
