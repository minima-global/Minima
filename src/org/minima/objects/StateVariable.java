package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class StateVariable implements Streamable {

	/**
	 * All possible state variable types
	 */
	public static final MiniByte STATETYPE_HEX 		= new MiniByte(1);
	public static final MiniByte STATETYPE_NUMBER 	= new MiniByte(2);
	public static final MiniByte STATETYPE_STRING 	= new MiniByte(4);
	public static final MiniByte STATETYPE_BOOL 	= new MiniByte(8);
	
	/**
	 * Create a particular State Variable
	 */
	public static StateVariable createStringStateVariable(int zPort, String zString, boolean zKeeper) {
		//Check within range
		if(zPort<0 || zPort>255) {
			throw new IllegalArgumentException("State Variable port MUST be 0-255");
		}
		
		//Create an empty vessel
		StateVariable sv 	= new StateVariable();
		sv.mPort 			= new MiniByte(zPort);
		sv.mType 			= STATETYPE_STRING;
		sv.mData 			= new MiniString(zString);
		sv.mKeepMMR 		= new MiniByte(zKeeper);
		
		return sv;
	}
	
	/**
	 * What type is this
	 */
	MiniByte mType;
	
	/**
	 * Positive Integer Number from 0-255
	 */
	MiniByte mPort;
	
	/**
	 * The data 
	 */
	MiniString mData; 
	
	/**
	 * Is this State variable a KEEPER - in the MMR
	 * A large signature would not be a keeper..
	 */
	MiniByte mKeepMMR;
	
	/**
	 * Constructors
	 */
	private StateVariable() {}
	
	public StateVariable(int zPort, String zData) {
		this(zPort, zData, true);
	}
	
	public StateVariable(int zPort, String zData, boolean zKeepMMR) {
		//Check within range
		if(zPort<0 || zPort>255) {
			throw new IllegalArgumentException("State Variable port MUST be 0-255");
		}
		
		//Store as MiniNumber
		mPort = new MiniByte(zPort);
		
		//Set the Data
		if(zData.toLowerCase().startsWith("mx")) {
			mData = new MiniString(Address.convertMinimaAddress(zData).to0xString());
			mType = STATETYPE_HEX;
		
		}else if(zData.toLowerCase().startsWith("0x")) {
			mData = new MiniString(new MiniData(zData).to0xString());
			mType = STATETYPE_HEX;
		
		}else if(zData.equalsIgnoreCase("true")) {
			mData = new MiniString("TRUE");
			mType = STATETYPE_BOOL;
		
		}else if(zData.equalsIgnoreCase("false")) {
			mData = new MiniString("FALSE");
			mType = STATETYPE_BOOL;
		
		}else if(zData.startsWith("[") && zData.endsWith("]")) {
			mData = new MiniString(zData);
			mType = STATETYPE_STRING;
		
		}else {
			mData = new MiniString(new MiniNumber(zData).toString());	
			mType = STATETYPE_NUMBER;
		}
		
		//Is this value kept in the MMR
		mKeepMMR = new MiniByte(zKeepMMR);
	}
	
	public int getPort() {
		return mPort.getValue();
	}
	
	public MiniByte getType() {
		return mType;
	}
	
	public MiniString getData() {
		return mData;
	}
	
	public boolean isKeepMMR() {
		return mKeepMMR.isTrue();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("port", mPort);
		ret.put("type", mType);
		ret.put("data", mData.toString());
		ret.put("keeper", mKeepMMR.isTrue());
		return ret;
	}
	
	@Override
	public String toString(){
		return mData.toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Port and Type
		mPort.writeDataStream(zOut);
		mType.writeDataStream(zOut);
		
		//Write the data in the correct format
		if(mType.isEqual(STATETYPE_BOOL)) {
			if(mData.isEqual("TRUE")) {
				MiniByte.TRUE.writeDataStream(zOut);
			}else {
				MiniByte.FALSE.writeDataStream(zOut);
			}
		}else if(mType.isEqual(STATETYPE_HEX)) {
			MiniData data = new MiniData(mData.toString());
			data.writeDataStream(zOut);
			
		}else if(mType.isEqual(STATETYPE_NUMBER)) {
			MiniNumber number = new MiniNumber(mData.toString());
			number.writeDataStream(zOut);
		
		}else if(mType.isEqual(STATETYPE_STRING)) {
			mData.writeDataStream(zOut);
		}
		
		mKeepMMR.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPort = MiniByte.ReadFromStream(zIn);
		mType = MiniByte.ReadFromStream(zIn);
		
		//What Data Type..
		if(mType.isEqual(STATETYPE_BOOL)) {
			MiniByte bool = MiniByte.ReadFromStream(zIn);
			if(bool.isTrue()) {
				mData = new MiniString("TRUE");
			}else {
				mData = new MiniString("FALSE");
			}
		}else if(mType.isEqual(STATETYPE_HEX)) {
			MiniData data = MiniData.ReadFromStream(zIn);
			mData = new MiniString(data.to0xString());
			
		}else if(mType.isEqual(STATETYPE_NUMBER)) {
			MiniNumber number = MiniNumber.ReadFromStream(zIn);
			mData = new MiniString(number.toString());
		
		}else if(mType.isEqual(STATETYPE_STRING)) {
			mData = MiniString.ReadFromStream(zIn);
		
		}
		
		mKeepMMR = MiniByte.ReadFromStream(zIn);
	}
	
	public static StateVariable ReadFromStream(DataInputStream zIn) throws IOException{
		StateVariable statevar = new StateVariable();
		statevar.readDataStream(zIn);
		return statevar;
	}
}
