package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
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
	 * Is this coin or BOTH children Unspendable
	 */
	public boolean mUnspendable = false;
	
	public static MMRData CreateMMRDataLeafNode(Streamable zData, MiniNumber zSumValue) {
		//Hash it.. USE 0 at start..
		MiniData hash = Crypto.getInstance().hashAllObjects(MiniNumber.ZERO, zData, zSumValue);
				
		//Create a new piece of data to add
		return new MMRData(hash, zSumValue);
	}
	
	public static MMRData CreateMMRDataParentNode(MMRData zLeft, MMRData zRight) {
		
		//Combine the Values..
		MiniNumber sumvalue   = zLeft.getValue().add(zRight.getValue());
				
		//Make the unique MMRData Hash - USE 1 at start
		MiniData combinedhash = Crypto.getInstance().hashAllObjects(MiniNumber.ONE,
																	zLeft.getData(),
																	zRight.getData(),
																	sumvalue);
				
		return new MMRData(combinedhash, sumvalue);
	}
	
	private MMRData() {}
	
	public MMRData(MiniData zHash, MiniNumber zValue) {
		mData = zHash;
		mValue = zValue; 
	}
	
	public MiniData getData() {
		return mData;
	}
	
	public MiniNumber getValue() {
		return mValue;
	}
	
	public void setUnspendable(boolean zUnspendable) {
		mUnspendable = zUnspendable;
	}
	
	public boolean isUnspendable(){
		return mUnspendable;
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
		mData.writeHashToStream(zOut);
		mValue.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData 	= MiniData.ReadHashFromStream(zIn);
		mValue	= MiniNumber.ReadFromStream(zIn);
	}
	
	public static MMRData ReadFromStream(DataInputStream zIn) throws IOException{
		MMRData data = new MMRData();
		data.readDataStream(zIn);
		return data;	
	}
}
