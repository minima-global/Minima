package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Random;

import org.minima.miniscript.Contract;
import org.minima.miniscript.values.ScriptValue;
import org.minima.utils.Crypto;
import org.minima.utils.Maths;

/**
 * This is a MiniData object with a fixed length of 32 bytes. 
 * 
 * If it is initialised with an array of less than 32 bytes in length it is zero padded. 
 * More it is truncated.
 * 
 * @author spartacus
 *
 */
public class MiniHash extends MiniData {

	/**
	 * The full length of this HASH
	 */
	public static final int HASH_LENGTH = 32;
	
	/**
	 * The Maximum HASH Value Possible
	 */
	public static final MiniHash MAX_HASH = new MiniHash(
							"0xFFFFFFFFFFFFFFFFFFFF"+
							  "FFFFFFFFFFFFFFFFFFFF"+
							  "FFFFFFFFFFFFFFFFFFFF"+
							  "FFFF");
	
	/**
	 * A zero filled hash
	 */
	public static MiniHash ZERO32 = new MiniHash("0x00");
	
	public MiniHash() {
		this("0x00");
	}
	
	public MiniHash(byte[] zData) {
		this(Maths.getDataAsString(zData));
	}
	
	public MiniHash(String zHex) {
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
		if(rlen<HASH_LENGTH){
			int diff = HASH_LENGTH-rlen;
			for(int i=0;i<diff;i++) {
				hex = "00"+hex;
			}
		}else if(len>HASH_LENGTH){
			hex = hex.substring(len-(HASH_LENGTH*2),len);	
		}
		
		return "0x"+hex.toUpperCase();
	}
	
	/**
	 * Get a random chunk of data
	 * 
	 * @param len
	 * @return
	 */
	public static MiniHash getRandomData() {
		Random rand = new Random();
		byte[] data = new byte[32];
		rand.nextBytes(data);
		return new MiniHash(data);
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
	
	public static MiniHash ReadFromStream(DataInputStream zIn){
		MiniHash data = new MiniHash();
		
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

		MiniHash hash32 = new MiniHash("0xeeccffffeeccff");
		
		MiniHash hhash32 = Crypto.getInstance().hashObject(hash32);
		
		System.out.println("Hash32 : "+hash32+" "+hash32.getLength()+" "+hhash32);
		System.out.println(hash32.getDataValue());
		
		byte[] hashdata = Crypto.getInstance().hashData(hash32.getData());
		
		hhash32 = new MiniHash(hashdata);
		
		System.out.println("Hash32 : "+hash32+" "+hash32.getLength()+" "+hhash32);
		System.out.println(hash32.getDataValue());
		
	}
	
}
