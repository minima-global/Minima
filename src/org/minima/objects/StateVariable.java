package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.kissvm.Contract;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniScript;
import org.minima.utils.BaseConverter;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class StateVariable implements Streamable {

	/**
	 * Byte for the port.. 0-255
	 * @param zData
	 */
	MiniByte mPort;
	
	/**
	 * The data can represent any of the value types used in script..
	 * HEX, Number or Script
	 */
	MiniScript mData; 
	
	/**
	 * Port and Data..
	 * 
	 * @param zPort
	 * @param zData
	 */
	public StateVariable(int zPort, String zData) {
		mPort	  = new MiniByte(zPort);
		
		//Cannot add Mx addresses.. only HEX addresses in SCRIPT
		if(zData.startsWith("Mx")) {
			//Convert to HEX
			mData = new MiniScript(Address.convertMinimaAddress(zData).to0xString());
		}else {
			mData = new MiniScript(zData);	
		}
	}
	
	private StateVariable() {}
	
	public void resetData(MiniScript zData) {
		mData = zData;
	}
	
	public MiniScript getData() {
		return mData;
	}
	
	public int getPort() {
		return mPort.getValue();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("port", mPort.toString());
		ret.put("data", mData.toString());
		return ret;
	}
	
	@Override
	public String toString(){
		return toJSON().toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPort.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPort = MiniByte.ReadFromStream(zIn);
		mData = MiniScript.ReadFromStream(zIn);
	}
	
	public static StateVariable ReadFromStream(DataInputStream zIn){
		StateVariable statevar = new StateVariable();
		
		try {
			statevar.readDataStream(zIn);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		
		return statevar;
	}
}
