package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

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
		//Convert script..
		mScript = new MiniString(zScript);
		
		//Set the Address..
		mAddressData = Crypto.getInstance().hashObject(mScript);
		
		//The Minima address as short as can be..
		mMinimaAddress = makeMinimaAddress(mAddressData);
	}
	
	public Address(MiniData zAddressData) {
		mScript         = new MiniString("");
		mAddressData 	= zAddressData;
		mMinimaAddress  = makeMinimaAddress(mAddressData);	
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
		mAddressData   	= MiniData.ReadHashFromStream(zIn);
		mScript        	= MiniString.ReadFromStream(zIn);
		mMinimaAddress  = makeMinimaAddress(mAddressData);		
	}
	
	public static Address ReadFromStream(DataInputStream zIn) throws IOException {
		Address addr = new Address();
		addr.readDataStream(zIn);
		return addr;
	}
	
	/**
	 * Convert an address into a Minima Checksum Base32 address
	 */
	public static String makeMinimaAddress(MiniData zAddress){
		//The Original data
		byte[] data = zAddress.getBytes();
		
		//First hash it to add some checksum digits..
		byte[] hash = Crypto.getInstance().hashData(data);
		
		//Now create one big byte array - address + first 4 bytes of hash
		byte[] tot16 = new byte[data.length + 4];
		
		//Copy the old..
		for(int i=0;i<data.length;i++) {
			tot16[i] = data[i];
		}
		
		//Add the checksum..
		for(int i=0;i<4;i++) {
			tot16[data.length+i] = hash[i];
		}
		
		//Now convert the whole thing to Base 32
		String b32 = BaseConverter.encode32(tot16);
		
		return b32;
	}
	
	public static MiniData convertMinimaAddress(String zMinimAddress) throws IllegalArgumentException {
		
		//First convert the whole thing back..
		byte[] decode = BaseConverter.decode32(zMinimAddress);
		
		//Now grab the fron and back..
		int len = decode.length;
		
		//Create the byte arrays
		byte[] checksum = new byte[4];
		byte[] data 	= new byte[len-4];
		
		//Copy correct..
		System.arraycopy(decode, 0, data, 0, len-4);
		System.arraycopy(decode, len-4, checksum, 0, 4);
		
		//Now check the hash
		byte[] hash = Crypto.getInstance().hashData(data);
		
		//Check the first 4 bytes..
		for(int i=0;i<4;i++) {
			if(hash[i] != checksum[i]) {
				throw new IllegalArgumentException("Invalid MxAddress - checksum wrong for "+zMinimAddress);
			}
		}
		
		return new MiniData(data);
	}
	
	public static void main(String[] zArgs) throws Exception {
		MiniData tt = MiniData.getRandomData(32);
		
		String madd 	= Address.makeMinimaAddress(tt);
		MiniData conv 	= Address.convertMinimaAddress(madd);
		
		System.out.println("Address   : "+tt.to0xString());
		System.out.println("MxAddress : "+madd);
		System.out.println("Converted : "+conv.to0xString());
		
	}
	
}
