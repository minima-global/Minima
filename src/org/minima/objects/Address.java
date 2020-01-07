package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Address implements Streamable{
	
	/**
	 * A default always true address.
	 */
	public static Address TRUE_ADDRESS = new Address("RETURN TRUE");
	
	/**
	 * The script that this address represents
	 */
	String mScript;
	
	/**
	 * The actual address hash in byte format
	 */
	MiniData32 mAddressData; 
	
	public Address() {}
		
	public Address(String zScript) {
		//Convert script..
		mScript = Contract.cleanScript(zScript);
		
		//Hash It..
		byte[] hdata = Crypto.getInstance().hashData(mScript.getBytes());
		
		//Set the Address..
		mAddressData = new MiniData32(hdata);
	}
	
	public Address(MiniData32 zAddressData) {
		mAddressData 	= zAddressData;
		mScript 		= "";
	}
	
	public JSONObject toJSON() {
		JSONObject addr = new JSONObject();
		addr.put("address", mAddressData.toString());
		addr.put("script", mScript);
		return addr;
	}
	
	@Override 
	public String toString() {
		return mAddressData.toString();
	}
	
	public String toFullString() {
		return toJSON().toString();
	}
	
	/**
	 * @return the script
	 */
	public String getScript() {
		return mScript;
	}
	
	public MiniData32 getAddressData() {
		return mAddressData;
	}

	public boolean isEqual(MiniData32 zAddress) {
		return mAddressData.isNumericallyEqual(zAddress);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mAddressData.writeDataStream(zOut);
		zOut.writeUTF(mScript);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mAddressData = MiniData32.ReadFromStream(zIn);
		mScript      = zIn.readUTF();
	}	
}
