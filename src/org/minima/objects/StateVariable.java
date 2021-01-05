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
	 * Byte for the port.. 0-255
	 * @param zData
	 */
	MiniNumber mPort;
	
	/**
	 * The data can represent any of the value types used in script..
	 * HEX, Number or Script
	 */
	MiniString mData; 
	
	/**
	 * Is this State variable a KEEPER - in the MMR
	 * A large signature would not be a keeper..
	 */
	MiniByte mKeepMMR;
	
	/**
	 * Port and Data and do you store long term..
	 * 
	 * @param zPort
	 * @param zData
	 */
	public StateVariable(MiniNumber zPort, String zData) {
		this(zPort, zData, MiniByte.TRUE);
	}
	
	public StateVariable(MiniNumber zPort, String zData, MiniByte zKeepMMR) {
		mPort = zPort;
		
		//Cannot add Mx addresses.. only HEX addresses in SCRIPT
		if(zData.startsWith("Mx")) {
			//Convert to HEX
			mData = new MiniString(Address.convertMinimaAddress(zData).to0xString());
		}else {
			mData = new MiniString(zData);	
		}
		
		//deafults to true
		mKeepMMR = zKeepMMR;
	}
	
	private StateVariable() {}
	
	public void resetData(MiniString zData, MiniByte zKeeper) {
		mData    = zData;
		mKeepMMR = zKeeper;
	}
	
	public MiniString getValue() {
		return mData;
	}
	
	public MiniNumber getPort() {
		return mPort;
	}
	
	public boolean isKeepMMR() {
		return mKeepMMR.isTrue();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("port", mPort.toString());
		ret.put("data", mData.toString());
		ret.put("keeper", mKeepMMR.isTrue());
		return ret;
	}
	
	@Override
	public String toString(){
		return toJSON().toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPort.writeDataStream(zOut);
		
		//Optimisation.. if is DATA
		if(mData.toString().startsWith("0x")) {
			//It's DATA.. might as well send it as that - half the size.
			MiniData data = new MiniData(mData.toString());
			
			MiniByte.TRUE.writeDataStream(zOut);
			data.writeDataStream(zOut);
		}else {
			//Just send it as normal.. number or script
			MiniByte.FALSE.writeDataStream(zOut);
			mData.writeDataStream(zOut);	
		}
		
		mKeepMMR.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPort = MiniNumber.ReadFromStream(zIn);
		
		//Was it sent as DATA or SCRIPT
		MiniByte isdata = MiniByte.ReadFromStream(zIn);
		if(isdata.isTrue()) {
			MiniData data = MiniData.ReadFromStream(zIn);
			mData = new MiniString(data.to0xString());
		}else {
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
