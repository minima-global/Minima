package org.minima.objects;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Arrays;

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
//	public static String makeMinimaAddress(MiniData zAddress){
//		//The Original data
//		byte[] data = zAddress.getBytes();
//		int addrlen = data.length;
//		
//		//First hash it to add some checksum digits..
//		byte[] hash = Crypto.getInstance().hashData(data);
//		
//		//Now create one big byte array - address + length + first 4 bytes of hash
//		byte[] tot16 = new byte[addrlen + 5];
//		
//		//Copy the old..
//		for(int i=0;i<data.length;i++) {
//			tot16[i] = data[i];
//		}
//		
//		//Add the length
//		tot16[data.length] = (byte)addrlen;
//		
//		//Add the checksum..
//		for(int i=0;i<4;i++) {
//			tot16[data.length+1+i] = hash[i];
//		}
//		
//		//Now convert the whole thing to Base 32
//		String b32 = BaseConverter.encode32(tot16);
//		
//		return b32;
//	}
//	
//	public static MiniData convertMinimaAddress(String zMinimAddress) throws IllegalArgumentException {
//		
//		//First convert the whole thing back..
//		byte[] decode 	= BaseConverter.decode32(zMinimAddress);
//		int len 		= decode.length;
//		int datalen 	= len-5;
//		
//		//The checksum is the last 4 bytes
//		byte[] checksum = new byte[4];
//		System.arraycopy(decode, len-4, checksum, 0, 4);
//		
//		//The actual data length
//		int origlen		= decode[len-5] & 0xFF;
//		byte[] data 	= new byte[origlen];
//		for(int i=0;i<origlen;i++) {
//			data[i] = 0;
//		}
//		
//		//Copy correct data..
//		System.arraycopy(decode, 0, data, origlen-datalen, datalen);
//		
//		//Now check the hash
//		byte[] hash = Crypto.getInstance().hashData(data);
//		
//		//Check the first 4 bytes..
//		for(int i=0;i<4;i++) {
//			if(hash[i] != checksum[i]) {
//				throw new IllegalArgumentException("Invalid MxAddress - checksum wrong for "+zMinimAddress);
//			}
//		}
//		
//		return new MiniData(data);
//	}
	
	/**
	 * Convert an address into a Minima Checksum Base32 address
	 * 
	 * MAX - 64K
	 */
	public static String makeMinimaAddress(MiniData zAddress){
		
		//The Original data
		byte[] data = zAddress.getBytes();
		int datalen = data.length;
		
		//First hash it to for checksum digits..
		byte[] hash 		= Crypto.getInstance().hashData(data);
		byte[] last4bytes	= new byte[4];
		for(int i=0;i<4;i++) {
			last4bytes[i] = hash[hash.length-4+i]; 
		}
		
		//Now write this info to stream
		ByteArrayOutputStream bos 	= new ByteArrayOutputStream();
	    DataOutputStream dos 		= new DataOutputStream(bos);
		
	    try {
	    	//MUST write 1 non 0 byte first to ensure no truncation in base 32 conversion
	    	dos.write(1);
	    	
	    	//the length 
			dos.writeShort(datalen);
			
		    //the data itself..
			dos.write(data);
			
			//the last 4 bytes of the hash
			dos.write(last4bytes);
			
			//Close the reams
			dos.close();
			bos.close();
		    
	    } catch (IOException e) {
			MinimaLogger.log(e);
			return null;
		}
	    
		//Get the bytes
		byte[] origdata = bos.toByteArray();
		
		//Now convert the whole thing to Base 32
		String b32 = BaseConverter.encode32(origdata);
		
		return b32;
	}
	
	public static MiniData convertMinimaAddress(String zMinimAddress) throws IllegalArgumentException {
		
		//First convert the whole thing back..
		byte[] decode 	= BaseConverter.decode32(zMinimAddress);

		//Now read in the data..
		ByteArrayInputStream bais = new ByteArrayInputStream(decode);
		DataInputStream dis = new DataInputStream(bais);
		
		byte[] data;
		byte[] checksum = new byte[4];
		
		try {
			//Read the first byte
			int one = dis.read();
			if(one!=1) {
				throw new IllegalArgumentException("Invalid MxAddress - should start with 1 "+zMinimAddress);
			}
			
	    	//First the data length
			int datalen = dis.readShort();
			
		    //the data itself..
			data = new byte[datalen];
			dis.readFully(data);
			
			//And the checksum
			dis.readFully(checksum); 
			
			//Close the reams
			dis.close();
			bais.close();
		    
	    } catch (IOException e) {
			MinimaLogger.log(e);
			return null;
		}
		
		//Now check the hash
		byte[] hash = Crypto.getInstance().hashData(data);
		
		//Check the first 4 bytes..
		for(int i=0;i<4;i++) {
			if(hash[hash.length-4+i] != checksum[i]) {
				throw new IllegalArgumentException("Invalid MxAddress - checksum wrong for "+zMinimAddress);
			}
		}
		
		return new MiniData(data);
	}
	
	public static void main(String[] zArgs) throws Exception {
		
		MiniData tt = MiniData.getRandomData(64);
//		MiniData tt = new MiniData("0x1");
		
		Address addr = new Address(tt);
		System.out.println("Address   : "+addr.toString());
		
		String madd 	= Address.makeMinimaAddress(tt);
		System.out.println("MxAddress : "+madd);
		
		MiniData conv 	= Address.convertMinimaAddress(madd);
		System.out.println("Converted : "+conv.to0xString());
		
	}
	
}
