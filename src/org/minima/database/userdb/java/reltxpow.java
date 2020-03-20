package org.minima.database.userdb.java;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class reltxpow implements Streamable {
	
	TxPOW 		mTxPow;
	MiniNumber 	mValue;
	
	public reltxpow() {}
	
	public reltxpow(TxPOW zTxPow, MiniNumber zValue) {
		mTxPow  = zTxPow;
		mValue  = zValue;
	}
	
	public TxPOW getTxPow() {
		return mTxPow;
	}
	
	public MiniNumber getChange() {
		return mValue;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("txpow", mTxPow.toJSON());
		ret.put("value", mValue);
		
		return ret;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTxPow.writeDataStream(zOut);
		mValue.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTxPow = new TxPOW();
		mTxPow.readDataStream(zIn);
		mValue = MiniNumber.ReadFromStream(zIn);
	}
}
