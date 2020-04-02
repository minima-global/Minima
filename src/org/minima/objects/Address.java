package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniData;
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
	MiniData mAddressData; 
	
	/**
	 * The SMALL format Minima Address..
	 */
	MiniData mSmallAddress; 
	
	public Address() {}
		
	public Address(String zScript) {
		//Convert script..
		mScript = Contract.cleanScript(zScript);
		
		//Hash It..
		byte[] hdata = Crypto.getInstance().hashData(mScript.getBytes());
		
		//Set the Address..
		mAddressData = new MiniData(hdata);
		
		//The small address
		mSmallAddress = new MiniData(Crypto.getInstance().hashData(mAddressData.getData(), 160)); 
	}
	
	public Address(MiniData zAddressData) {
		mAddressData 	= zAddressData;
		
		if(mAddressData.getLength() == 64) {
			mSmallAddress = new MiniData(Crypto.getInstance().hashData(mAddressData.getData(), 160));
		}else {
			mSmallAddress = mAddressData;
		}
		
		mScript = "";
	}
	
	public JSONObject toJSON() {
		JSONObject addr = new JSONObject();
		addr.put("script", mScript);
		addr.put("address", mAddressData.toString());
		addr.put("miniaddress", mSmallAddress.toString());
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
	
	public MiniData getAddressData() {
		return mAddressData;
	}

	public boolean isEqual(MiniData zAddress) {
		return mAddressData.isEqual(zAddress) || mSmallAddress.isEqual(zAddress);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mAddressData.writeDataStream(zOut);
		zOut.writeUTF(mScript);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mAddressData  = MiniData.ReadFromStream(zIn);
		mScript       = zIn.readUTF();
		
		if(mAddressData.getLength() == 64) {
			mSmallAddress = new MiniData(Crypto.getInstance().hashData(mAddressData.getData(), 160));
		}else {
			mSmallAddress = mAddressData;
		}
	}	
}
