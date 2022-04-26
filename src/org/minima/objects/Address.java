package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
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
		int addrlen = data.length;
		
		//First hash it to add some checksum digits..
		byte[] hash = Crypto.getInstance().hashData(data);
		
		//Now create one big byte array - address + length + first 4 bytes of hash
		byte[] tot16 = new byte[addrlen + 5];
		
		//Copy the old..
		for(int i=0;i<data.length;i++) {
			tot16[i] = data[i];
		}
		
		//Add the length
		tot16[data.length] = (byte)addrlen;
		
		//Add the checksum..
		for(int i=0;i<4;i++) {
			tot16[data.length+1+i] = hash[i];
		}
		
		//Now convert the whole thing to Base 32
		String b32 = BaseConverter.encode32(tot16);
		
		return b32;
	}
	
	public static MiniData convertMinimaAddress(String zMinimAddress) throws IllegalArgumentException {
		
		//First convert the whole thing back..
		byte[] decode 	= BaseConverter.decode32(zMinimAddress);
		int len 		= decode.length;
		int datalen 	= len-5;
		
		//The checksum is the last 4 bytes
		byte[] checksum = new byte[4];
		System.arraycopy(decode, len-4, checksum, 0, 4);
		
		//The actual data length
		int origlen		= decode[len-5] & 0xFF;
		byte[] data 	= new byte[origlen];
		for(int i=0;i<origlen;i++) {
			data[i] = 0;
		}
		
		//Copy correct data..
		System.arraycopy(decode, 0, data, origlen-datalen, datalen);
		
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
//		MiniData tt = MiniData.getRandomData(160);
		
		
		MiniData tt = new MiniData("0x00000000FFFEEFF00");
		Address addr = new Address(tt);
		System.out.println("Address   : "+tt.to0xString()+" "+addr.toString());
		
		String madd 	= Address.makeMinimaAddress(tt);
		System.out.println("MxAddress : "+madd);
		
		MiniData conv 	= Address.convertMinimaAddress(madd);
		System.out.println("Converted : "+conv.to0xString());
		
	}
	
}
