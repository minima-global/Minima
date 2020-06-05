package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.GlobalParams;
import org.minima.kissvm.Contract;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.BaseConverter;
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
	MiniString mScript;
	
	/**
	 * The actual address hash in byte format
	 */
	MiniData mAddressData; 
	
	/**
	 * The Minima Mx address that has error detection and uses base 32!
	 */
	String mMinimaAddress;
	
	public Address() {}
	
	public Address(String zScript) {
		this(zScript, GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH);
	}
	
	public Address(String zScript, int zBitLength) {
		//Clean the script up..
		String cleanscript = Contract.cleanScript(zScript);
		
		//Convert script..
		mScript = new MiniString(cleanscript);
		
		//Set the Address..
		mAddressData = new MiniData(Crypto.getInstance().hashData(mScript.getData(),zBitLength));
		
		//The Minima address as short as can be..
		mMinimaAddress = makeMinimaAddress(mAddressData);
	}
	
	public Address(MiniData zAddressData) {
		mScript         = new MiniString("");
		mAddressData 	= zAddressData;
		
		if(mAddressData.getLength()<20) {
			mMinimaAddress  = mAddressData.to0xString();
		}else {
			mMinimaAddress  = makeMinimaAddress(mAddressData);	
		}
	}
	
	public JSONObject toJSON() {
		JSONObject addr = new JSONObject();
		addr.put("script", mScript.toString());
		addr.put("hexaddress", mAddressData.toString());
		addr.put("miniaddress", mMinimaAddress);
		return addr;
	}
	
	@Override 
	public String toString() {
		return mAddressData.toString();
	}
	
	/**
	 * @return the script
	 */
	public String getScript() {
		return mScript.toString();
	}
	
	public MiniData getAddressData() {
		return mAddressData;
	}
	
	public String getMinimaAddress() {
		return mMinimaAddress;
	}
	
	public boolean isEqual(Address zAddress) {
		return mAddressData.isEqual(zAddress.getAddressData());
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mAddressData.writeHashToStream(zOut);
		mScript.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mAddressData   = MiniData.ReadHashFromStream(zIn);
		mScript        = MiniString.ReadFromStream(zIn);
		
		if(mAddressData.getLength()<20) {
			mMinimaAddress  = mAddressData.to0xString();
		}else {
			mMinimaAddress  = makeMinimaAddress(mAddressData);	
		}	
	}
	
	/**
	 * Convert an address into a Minima Checksum Base32 address
	 * 
	 * @param zAddress
	 * @return the address
	 */
	public static String makeMinimaAddress(MiniData zAddress) throws ArithmeticException {
		//The Original data
		byte[] data = zAddress.getData();
		
		//First hash it to add some checksum digits..
		byte[] hash = Crypto.getInstance().hashData(data, 160);
		
		//Calculate a new length - ONLY certain lengths allowed!
		int len    = data.length;
		int newlen = 0;
		
		//160 bit
		if(len == 20) {
			newlen = 25;

		//224 bit
		}else if(len == 28) {
			newlen = 30;
				
		//256 bit
		}else if(len == 32) {
			newlen = 35;
		
		//288 bit
		}else if(len == 36) {
			newlen = 40;
		
		//320 bit
		}else if(len == 40) {
			newlen = 45;
		
		//384 bit
		}else if(len == 48) {
			newlen = 50;
		
		//416 bit
		}else if(len == 52) {
			newlen = 55;
		
		//448 bit
		}else if(len == 56) {
			newlen = 60;
		
		//480 bit
		}else if(len == 60) {
			newlen = 65;
							
		//512 bit
		}else if(len == 64) {
			newlen = 70;
		
		}else {
			throw new ArithmeticException("ERROR - Make Minima Address : not a valid length address!");
		}
		
		int nbytes = newlen - len;
		
		//Add the first 4 digits..
		byte[] addr = new byte[len+nbytes];
		
		//Copy the old..
		for(int i=0;i<len;i++) {
			addr[i] = data[i];
		}
		
		//Add the checksum..
		for(int i=0;i<nbytes;i++) {
			addr[len+i] = hash[i];
		}
		
		//Now convert the whole thing to Base 32
		String b32 = BaseConverter.encode32(addr);
		
		return "Mx"+b32;
	}

	/**
	 * Convert and check a Minima address..
	 * @param zMinimaAddress
	 * @return
	 */
	public static MiniData convertMinimaAddress(String zMinimaAddress) throws ArithmeticException {
		if(!zMinimaAddress.startsWith("Mx")) {
			throw new ArithmeticException("Minima Addresses must start with Mx");
		}
		
		//Get the data
		byte[] data = BaseConverter.decode32(zMinimaAddress.substring(2)); 
		
		int len    = data.length;
		int bitlen = 0; 
		
		//Convert back..
		if(len == 25) {
			bitlen = 20;
		}else if(len == 30) {
			bitlen = 28;
		}else if(len == 35) {
			bitlen = 32;
		}else if(len == 40) {
			bitlen = 36;
		}else if(len == 45) {
			bitlen = 40;
		}else if(len == 50) {
			bitlen = 48;
		}else if(len == 55) {
			bitlen = 52;
		}else if(len == 60) {
			bitlen = 56;
		}else if(len == 65) {
			bitlen = 60;
		}else if(len == 70) {
			bitlen = 64;
		}else {
			throw new ArithmeticException("Wrong length Minima Address : "+len);
		}
		
		int hashlen = len - bitlen;
		byte[] newdata = new byte[bitlen];
		
		//Copy the old..
		for(int i=0;i<bitlen;i++) {
			newdata[i] = data[i];
		}
		
		//Now Hash it.. 
		byte[] hash = Crypto.getInstance().hashData(newdata, 160);
				
		//Check it with the checksum..
		for(int i=0;i<hashlen;i++) {
			if(hash[i] != data[i+bitlen]) {
				throw new ArithmeticException("Minima Address Checksum Error");	
			}
		}
		
		return new MiniData(newdata);
	}
	
}
