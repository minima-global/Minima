package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
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
	MiniHash mAddressData; 
	
	public Address() {}
		
	public Address(String zScript) {
		//Convert script..
		mScript = Contract.cleanScript(zScript);
		
		//Hash It..
		byte[] hdata = Crypto.getInstance().hashData(mScript.getBytes());
		
		//Set the Address..
		mAddressData = new MiniHash(hdata);
	}
	
	public Address(MiniHash zAddressData) {
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
	
	public MiniHash getAddressData() {
		return mAddressData;
	}

	public boolean isEqual(MiniHash zAddress) {
		return mAddressData.isNumericallyEqual(zAddress);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mAddressData.writeDataStream(zOut);
		zOut.writeUTF(mScript);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mAddressData = MiniHash.ReadFromStream(zIn);
		mScript      = zIn.readUTF();
	}	
}
