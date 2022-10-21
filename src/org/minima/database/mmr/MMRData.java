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
	
	public static MMRData CreateMMRDataLeafNode(Streamable zData, MiniNumber zSumValue) {
		//Hash it..
		MiniData hash = Crypto.getInstance().hashObject(zData);
				
		//Create a new piece of data to add
		return new MMRData(true, hash, zSumValue);
//		return new MMRData(hash, zSumValue);
	}
	
	public static MMRData CreateParentMMRData( MMRData zLeft, MMRData zRight) {
		
		//Combine the Values..
		MiniNumber sumvalue   = zLeft.getValue().add(zRight.getValue());
				
		//Make the unique MMRData Hash
		MiniData combinedhash = Crypto.getInstance().hashAllObjects(zLeft.getData(),
																	zRight.getData(),
																	sumvalue);
				
		return new MMRData(true, combinedhash, sumvalue);
	}
	
	private MMRData() {}
	
//	public MMRData(MiniData zHash) {
//		this(zHash, MiniNumber.ZERO); 
//	}
	
//	public MMRData(MiniData zHash, MiniNumber zValue) {
	public MMRData(boolean zRaw, MiniData zHash, MiniNumber zValue) {
		mData = zHash;
		mValue = zValue; 
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
		mValue.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData 	= MiniData.ReadFromStream(zIn);
		mValue	= MiniNumber.ReadFromStream(zIn);
	}
	
	public static MMRData ReadFromStream(DataInputStream zIn) throws IOException{
		MMRData data = new MMRData();
		data.readDataStream(zIn);
		return data;	
	}
}
