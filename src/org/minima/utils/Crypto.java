/**
 * 
 */
package org.minima.utils;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.minima.objects.base.MiniData;
import org.minima.utils.digest.Digest;
import org.minima.utils.digest.KeccakDigest;
import org.minima.utils.digest.SHA256Digest;

/**
 * @author Spartacus Rex
 *
 */
public class Crypto {

	public static final BigInteger MAX_VAL = new BigInteger(
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF", 16); 
	
	public static final MiniData MAX_HASH = new MiniData(
					"0xFFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF");
	
	/**
	 * 1 Mega Hash - for now 100,000 - just to test..
	 */
	public static final BigInteger MEGA_VAL = MAX_VAL.divide(new BigInteger("100000"));	
	public static final MiniData MEGA_HASH  = new MiniData("0x"+MEGA_VAL.toString(16));
	
	
	/**
	 * Get the default instance..
	 */
	private static Crypto mCrypto;
	public static Crypto getInstance(){
		if(mCrypto==null){
			mCrypto = new Crypto();	
		}
		return mCrypto;
	}
	
	public Crypto(){

//		Provider[] provs = Security.getProviders();
//		for(int i=0;i<provs.length;i++){
//			SimpleLogger.log("Provider "+provs[i].getInfo());
//		}
		
//		try {
//			mDigest = MessageDigest.getInstance("SHA-256");
////			mDigest = MessageDigest.getInstance("SHA1");
////			mDigest = MessageDigest.getInstance("MD5");
//		} catch (NoSuchAlgorithmException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
	}
	
//	private MessageDigest getDigest() throws NoSuchAlgorithmException {
//		return MessageDigest.getInstance("SHA-256");
//	}
	
	public byte[] hashData(byte[] zData){
		return hashData(zData, 512);
	}
	
	public byte[] hashData(byte[] zData, int zBitLength){
		try {
			//Bouncy..
			Digest keccak = new KeccakDigest(zBitLength);
			byte[] output = new byte[keccak.getDigestSize()];
			keccak.update(zData, 0, zData.length);
			keccak.doFinal(output, 0);
			return output;
		}catch(Exception exc) {
			exc.printStackTrace();
		}
		return null;
	}
	
//	public byte[] hash(byte[] zLeft, byte[] zRight ){
//		//Join the 2 arrays..
//		byte[] joined = new byte[zData1.length+zData2.length];
//		
//		//Copy over..
//		System.arraycopy(zData1, 0, joined, 0, zData1.length);
//		System.arraycopy(zData2, 0, joined, zData1.length, zData2.length);
//		
//		//Now Hash that..
//		return mDigest.digest(joined);
//	}
		
	public byte[] hashSHA2(byte[] zData){
		try {
			//Bouncy..
			Digest sha2 = new SHA256Digest();
			byte[] output = new byte[sha2.getDigestSize()];
			sha2.update(zData, 0, zData.length);
			sha2.doFinal(output, 0);
			
			return output;
			
			//Do it..
//			return getDigest().digest(zData);
		}catch(Exception exc) {
			exc.printStackTrace();
		}
		return null;
	}

	public MiniData hashObject(Streamable zObject) {
		return hashObject(zObject, 512);
	}
	
	public MiniData hashObject(Streamable zObject, int zBitLength) {
		try {
			//Get the Data..
			ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
			DataOutputStream dos 		= new DataOutputStream(baos);
			
			//Write the Object to the Stream
			zObject.writeDataStream(dos);
			
			//Flush the stream
			dos.flush();
			
			//Get the Data..
			byte[] objdata = baos.toByteArray();
			
			//Hash That
			byte[] hashdata = hashData(objdata,zBitLength);
			
			return new MiniData(hashdata);
		
		}catch (Exception e) {
			//Error Hashing!?
			e.printStackTrace();
		}
		
		return null;
	}
	
	public MiniData hashObjects(Streamable zLeftObject, Streamable zRightObject2) {
		return hashObjects(zLeftObject, zRightObject2, 512);
	}
	
	public MiniData hashObjects(Streamable zLeftObject, Streamable zRightObject2, int zBitLength) {
		try {
			//Get the Data..
			ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
			DataOutputStream dos 		= new DataOutputStream(baos);
			
			//Write the First Object to the Stream
			zLeftObject.writeDataStream(dos);
		
			//And now the second object
			zRightObject2.writeDataStream(dos);
			
			//Flush the stream
			dos.flush();
			
			//Get the Data..
			byte[] objdata = baos.toByteArray();
			
			//Hash That
			byte[] hashdata = hashData(objdata,zBitLength);
		
			//Final Answer
			return new MiniData(hashdata);
		
		}catch (Exception e) {
			e.printStackTrace();
			//Error Hashing!?
			//TODO
		}
		
		return null;
	}
	
	public MiniData hashAllObjects(Streamable... zObjects) {
		try {
			//Get the Data..
			ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
			DataOutputStream dos 		= new DataOutputStream(baos);
			
			for(Streamable object : zObjects) {
				//Write to the stream
				object.writeDataStream(dos);
			}
				
			//Flush the stream
			dos.flush();
			
			//Get the Data..
			byte[] objdata = baos.toByteArray();
			
			//Hash That
			byte[] hashdata = hashData(objdata);
		
			//Final Answer
			return new MiniData(hashdata);
		
		}catch (Exception e) {
			e.printStackTrace();
			//Error Hashing!?
			//TODO
		}
		
		return null;
	}
	
	
	public static void main(String[] zArgs) {
		
//		byte[] data = "Hello".getBytes();
//		
//		MiniData32 trest = new MiniData32("Hello You!");
//		
//		//Now Hash..
//		try {
//			Digest keccak = new SHA256Digest();
//			byte[] output = new byte[keccak.getDigestSize()];
//			keccak.update(trest.getData(), 0, trest.getLength());
//			keccak.doFinal(output, 0);
//			
//			System.out.println(new MiniData32(output));
//			
//			byte[] hash = Crypto.getInstance().hashObject(trest).getData();
//			
//			System.out.println(new MiniData32(hash));
//			
//		}catch(Exception exc) {
//			exc.printStackTrace();
//		}
		
		
	}
}
