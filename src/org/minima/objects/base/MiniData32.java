package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Random;

import org.minima.utils.Crypto;
import org.minima.utils.Maths;

/**
 * This is a RamData object with a fixed length of 32 bytes. 
 * 
 * If it is initialised with an array of less than 32 bytes in length it is zero padded. 
 * More it is truncated.
 * 
 * @author spartacus
 *
 */
public class MiniData32 extends MiniData {

	/**
	 * A zero filled hash
	 */
	public static MiniData32 ZERO32 = new MiniData32("0x00");
	
	
	private static final int MAX_LENGTH = 32;
	
	public MiniData32() {
		this("0x00");
	}
	
	public MiniData32(byte[] zData) {
		this(Maths.getDataAsString(zData));
	}
	
	public MiniData32(String zHex) {
		super(ensureSize(zHex));
	}
	
	public static String ensureSize(String zHEX) {
		String hex = zHEX;
		if(hex.startsWith("0x")) {
			hex = zHEX.substring(2);
		}	
		
		int len = hex.length();
		
		//Must be 2 digits per byte
		if(len % 2 != 0) {
			//Need a leading zero
			hex="0"+hex;
			len = hex.length();
		}
		
		//Now how long are we ? MUST be 64 bytes..
		int rlen = len/2;
		if(rlen<MAX_LENGTH){
			int diff = MAX_LENGTH-rlen;
			for(int i=0;i<diff;i++) {
				hex = "00"+hex;
			}
		}else if(len>MAX_LENGTH){
			hex = hex.substring(len-(MAX_LENGTH*2),len);	
		}
		
		return "0x"+hex;
	}
	
	/**
	 * Get a random chunk of data
	 * 
	 * @param len
	 * @return
	 */
	public static MiniData32 getRandomData() {
		Random rand = new Random();
		byte[] data = new byte[32];
		rand.nextBytes(data);
		return new MiniData32(data);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.write(mData);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData = new byte[32];
		zIn.readFully(mData);
		
		//Set the data value
		setDataValue();
	}
	
	public static MiniData32 ReadFromStream(DataInputStream zIn){
		MiniData32 data = new MiniData32();
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
	
	public static void main(String[] zArgs) {

		MiniData32 hash32 = new MiniData32("0xeeccffffeeccff");
		
		MiniData32 hhash32 = Crypto.getInstance().hashObject(hash32);
		
		System.out.println("Hash32 : "+hash32+" "+hash32.getLength()+" "+hhash32);
		System.out.println(hash32.getDataVaue());
		
		byte[] hashdata = Crypto.getInstance().hashData(hash32.getData());
		
		hhash32 = new MiniData32(hashdata);
		
		System.out.println("Hash32 : "+hash32+" "+hash32.getLength()+" "+hhash32);
		System.out.println(hash32.getDataVaue());
		
	}
	
}
