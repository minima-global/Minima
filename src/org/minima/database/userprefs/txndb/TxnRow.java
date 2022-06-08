package org.minima.database.userprefs.txndb;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TxnRow implements Streamable {

	public String mID;
	public Transaction 	mTransaction;
	public Witness 		mWitness;
	
	private TxnRow() {}
	
	public TxnRow(String zID, Transaction zTransaction, Witness zWitness) {
		mID 			= zID;
		mTransaction 	= zTransaction;	
		mWitness		= zWitness;	
	}
	
	public void setID(String zID) {
		mID = zID;
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

	/**
	 * Convert a MiniData version into a TxnRow
	 */
	public static TxnRow convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		TxnRow txnrow = null;
		
		try {
			//Convert data
			txnrow = TxnRow.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return txnrow;
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
	
	public static TxnRow ReadFromStream(DataInputStream zIn) throws IOException {
		TxnRow txp = new TxnRow();
		txp.readDataStream(zIn);
		return txp;
	}
}
