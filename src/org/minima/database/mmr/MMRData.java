package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MMRData implements Streamable{
	
	/**
	 * The Hash of the Data or hash of the children
	 */
	private MiniData mData;
	
	/**
	 * The Amount - this is a hash SUM tree. The value of the sum of the children
	 */
	private MiniNumber mValue;
	
	/**
	 * Are we a hashsum tree or just a hash tree
	 */
	boolean mHashSum = true;
	
	private MMRData() {}
	
	public MMRData(MiniData zHash) {
		this(zHash, MiniNumber.ZERO); 
	}
	
	public MMRData(MiniData zHash, MiniNumber zValue) {
		mData = zHash;
		mValue = zValue; 
	}
	
	public void setHashSum(boolean zHashSum) {
		mHashSum = zHashSum;
	}
	
	public MiniData getData() {
		return mData;
	}
	
	public MiniNumber getValue() {
		return mValue;
	}
	
	public boolean isEqual(MMRData zData) {
		return mData.isEqual(zData.getData()) && mValue.isEqual(zData.getValue());
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		obj.put("data", mData.to0xString());
		obj.put("value", mValue.toString());
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mData.writeDataStream(zOut);
		
		if(mHashSum) {
			mValue.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData 	= MiniData.ReadFromStream(zIn);
		
		if(mHashSum) {
			mValue	= MiniNumber.ReadFromStream(zIn);
		}else {
			mValue	= MiniNumber.ZERO;
		}
	}
	
	public static MMRData ReadFromStream(boolean zHashSum, DataInputStream zIn) throws IOException{
		MMRData data = new MMRData();
		data.setHashSum(zHashSum);
		data.readDataStream(zIn);
		return data;	
	}
}
