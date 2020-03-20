package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.miniscript.values.Value;
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
	MiniByte mPort;
	
	/**
	 * The data can represent any of the value types used in script..
	 * HEX, Number or Script
	 */
	MiniString mData; 
	
	/**
	 * Port and Data..
	 * 
	 * @param zPort
	 * @param zData
	 */
	public StateVariable(int zPort, String zData) {
		mPort	  = new MiniByte(zPort);
		mData     = new MiniString(Contract.cleanScript(zData));
	}
	
	private StateVariable() {}
	
	public void resetData(MiniString zData) {
		mData = zData;
	}
	
	public MiniString getData() {
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
		mData = MiniString.ReadFromStream(zIn);
	}
	
	public static StateVariable ReadFromStream(DataInputStream zIn){
		StateVariable statevar = new StateVariable();
		
		try {
			statevar.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return statevar;
	}
}
