/**
 * 
 */
package org.minima.utils;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.bouncycastle.crypto.Digest;
import org.bouncycastle.crypto.digests.SHA256Digest;
import org.bouncycastle.crypto.digests.SHA3Digest;
import org.minima.objects.base.MiniData;

/**
 * @author Spartacus Rex
 *
 */
public class Crypto {

	/**
	 * Largest Number
	 */
	public static final BigInteger MAX_VAL = new BigInteger(
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF", 16);
	
	public static BigDecimal MAX_VALDEC = new BigDecimal(MAX_VAL);
	
	/**
	 * Largest HEX value
	 */
	public static final MiniData MAX_HASH = new MiniData(MAX_VAL);
	
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
	
	public Crypto(){}

	/**
	 * SHA 2
	 * @param zData
	 * @return
	 */
	public byte[] hashSHA2(byte[] zData){
		try {
			
			//Bouncy..
			Digest sha2 = new SHA256Digest();
			byte[] output = new byte[sha2.getDigestSize()];
			sha2.update(zData, 0, zData.length);
			sha2.doFinal(output, 0);
			return output;
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		return null;
	}
	
	/**
	 * SHA 3
	 * The DEFAULT
	 */
	public byte[] hashData(byte[] zData){
		try {
			
			//Bouncy..
			Digest sha3 = new SHA3Digest(256);
			byte[] output = new byte[sha3.getDigestSize()];
			sha3.update(zData, 0, zData.length);
			sha3.doFinal(output, 0);
			return output;
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		return null;
	}

	/**
	 * 
	 * @param zObject
	 * @return
	 */
	public MiniData hashObject(Streamable zObject) {
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
			byte[] hashdata = hashData(objdata);
			
			MiniData ret = new MiniData(hashdata);
			
			dos.close();
			baos.close();
			
			return ret;
		
		}catch (Exception e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
	
	public MiniData hashObjects(Streamable zLeftObject, Streamable zRightObject2) {
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
			byte[] hashdata = hashData(objdata);
		
			MiniData ret = new MiniData(hashdata);
			
			dos.close();
			baos.close();
			
			return ret;
		
		}catch (Exception e) {
			MinimaLogger.log(e);
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
		
			MiniData ret = new MiniData(hashdata);
			
			dos.close();
			baos.close();
			
			return ret;
					
		}catch (Exception e) {
			MinimaLogger.log(e);
		}
		
		return null;
	}
}
