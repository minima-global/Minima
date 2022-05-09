/**
 * 
 */
package org.minima.objects.base;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Arrays;

import org.minima.utils.BaseConverter;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

/**
 * @author Spartacus Rex
 *
 */
public class MiniData implements Streamable {
	
	public static int MINIMA_MAX_HASH_LENGTH 		= 64;
	
	/**
	 * 256 MB Max MiniData size
	 */
	public static int MINIMA_MAX_MINIDATA_LENGTH 	= 1024 * 1024 * 256;
	
	public static final MiniData ZERO_TXPOWID 	= new MiniData("0x00");
	public static final MiniData ONE_TXPOWID 	= new MiniData("0x01");
	
	/**
	 * The byte data
	 */
	protected byte[] mData;
	
	/**
	 * The BigInteger version
	 */
	BigInteger mDataValue = null;
	
	/**
	 * Default Empty Constructor
	 */
	public MiniData() {
		this(new byte[0]);
	}
	
	/**
	 * Throws a NumberFormatException if the HEX String is Invalid
	 * 
	 * @param zHexString
	 */
	public MiniData(String zHexString) {
		this(BaseConverter.decode16(zHexString));
	}
	
	/**
	 * Construct from a byte array
	 * 
	 * @param zData
	 */
	public MiniData(byte[] zData) {
		mData = zData;
	}
	
	/**
	 * Make sure is at least Min Length in size
	 */
	public MiniData(byte[] zData, int zMinLength) {
		
		//How big is it
		int len = zData.length;
		
		if(len<zMinLength) {
			byte[] data = new byte[zMinLength];
			
			//And the rest..
			int diff = zMinLength - len;
			for(int i=0;i<diff;i++) {
				data[i] = 0;
			}
			
			for(int i=0;i<len;i++) {
				data[i+diff] = zData[i];
			}
			
			mData = data;
		}else {
			mData = zData;
		}
	}
	
	/**
	 * Use a BigInteger to create
	 * 
	 * @param zBigInteger
	 */
	public MiniData(BigInteger zBigInteger) {
		//Only Positive numbers
		if(zBigInteger.signum() == -1) {
			throw new IllegalArgumentException("MiniData value must be a postitive BigInteger");
		}
		
		//Get the Byte array
		byte[] signedValue = zBigInteger.toByteArray();
        
		//Remove a leading zero..
		if(signedValue.length>1 && signedValue[0] == 0x00) {
			mData = Arrays.copyOfRange(signedValue, 1, signedValue.length);
        }else {
        	mData = signedValue;
        }
	}
	
	public int getLength() {
		return mData.length;
	}
	
	public byte[] getBytes() {
		return mData;
	}
	
	public BigInteger getDataValue() {
		if(mDataValue == null) {
			mDataValue = new BigInteger(1,mData);
		}
		
		return mDataValue;
	}
	
	public BigDecimal getDataValueDecimal() {
		return new BigDecimal(getDataValue());
	}
	
	@Override
	public boolean equals(Object o) {
		return isEqual((MiniData)o);
	}
	
	public boolean isEqual(MiniData zCompare) {
		int len = getLength();
		if(len != zCompare.getLength()) {
			return false;
		}
		
		//Get both data sets..
		byte[] data = zCompare.getBytes();
		
		//Check the data..
		for(int i=0;i<len;i++) {
			if(data[i] != mData[i]) {
				return false;
			}
		}
	
		return true;
	}
	
	public boolean isLess(MiniData zCompare) {
		return getDataValue().compareTo(zCompare.getDataValue()) < 0;
	}
	
	public boolean isLessEqual(MiniData zCompare) {
		return getDataValue().compareTo(zCompare.getDataValue()) <= 0;
	}
	
	public boolean isMore(MiniData zCompare) {
		return getDataValue().compareTo(zCompare.getDataValue()) > 0;
	}
	
	public boolean isMoreEqual(MiniData zCompare) {
		return getDataValue().compareTo(zCompare.getDataValue()) >= 0;
	}
	
	public MiniData shiftr(int zNumber) {
		return new MiniData(getDataValue().shiftRight(zNumber));
	}
	
	public MiniData shiftl(int zNumber) {
		return new MiniData(getDataValue().shiftLeft(zNumber));
	}
	
	public int compare(MiniData zCompare) {
		return getDataValue().compareTo(zCompare.getDataValue());
	}
	
	public MiniData concat(MiniData zConcat) {
		int locallen  = getLength();
		int concatlen = zConcat.getBytes().length; 
				
		int totlen   = locallen+concatlen;
		byte[] total = new byte[totlen]; 
		
		//First copy local..
		System.arraycopy(getBytes(), 0, total, 0, locallen);
		
		//Then the new..
		System.arraycopy(zConcat.getBytes(), 0, total, locallen, concatlen);
		
		return new MiniData(total);
	}
	
	@Override
	public String toString() {
		return to0xString();
	}
	
	public String to0xString() {
		return BaseConverter.encode16(mData);
	}
	
	public String to0xString(int zLen) {
		String data = to0xString();
		int len = data.length();
		if(len > zLen) {
			len = zLen;
		}
		return data.substring(0, len).concat("..");
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeInt(mData.length);
		zOut.write(mData);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		readDataStream(zIn, -1);
	}
	
	/**
	 * Read in a specific amount.. when receiving messages over the network
	 * 
	 * @param zIn
	 * @param zSize
	 * @throws IOException
	 */
	public void readDataStream(DataInputStream zIn, int zSize) throws IOException {
		int len = zIn.readInt();
		
		//Check against maximum allowed
		if(len > MINIMA_MAX_MINIDATA_LENGTH) {
			throw new IOException("Read Error : MiniData Length larger than maximum allowed (256 MB) "+MiniFormat.formatSize(len));
		}
		
		if(zSize != -1) {
			if(len != zSize) {
				throw new IOException("Read Error : MiniData length not correct as specified "+zSize);
			}
		}
		
		mData = new byte[len];
		zIn.readFully(mData);
	}
	
	public static MiniData ReadFromStream(DataInputStream zIn) throws IOException{
		return ReadFromStream(zIn, -1);
	}
	
	/**
	 * Specify Size - as network input can be dangerous..
	 * @param zIn
	 * @param zSize
	 * @return
	 * @throws IOException 
	 */
	public static MiniData ReadFromStream(DataInputStream zIn, int zSize) throws IOException{
		MiniData data = new MiniData();
		data.readDataStream(zIn, zSize);
		return data;
	}

	/**
	 * Special Functions to input output HASH data..
	 */
	public void writeHashToStream(DataOutputStream zOut) throws IOException {
		if(mData.length > MINIMA_MAX_HASH_LENGTH) {
			throw new IOException("Write Error : HASH Length greater than "+MINIMA_MAX_HASH_LENGTH+"! "+mData.length);
		}
		
		zOut.writeInt(mData.length);
		zOut.write(mData);
	}

	public void readHashFromStream(DataInputStream zIn) throws IOException {
		int len = zIn.readInt();
		if(len > MINIMA_MAX_HASH_LENGTH) {
			throw new IOException("Read Error : HASH Length greater than "+MINIMA_MAX_HASH_LENGTH+"! "+len);
		}else if(len<0) {
			throw new IOException("Read Error : HASH Length less than 0! "+len);
		}
		
		mData = new byte[len];
		zIn.readFully(mData);
	}
	
	public static MiniData ReadHashFromStream(DataInputStream zIn) throws IOException{
		MiniData data = new MiniData();
		data.readHashFromStream(zIn);
		return data;
	}
	
	public static void WriteToStream(DataOutputStream zOut, byte[] zData) throws IOException{
		new MiniData(zData).writeDataStream(zOut);
	}
	
	public static MiniData getMiniDataVersion(Streamable zObject) {
		ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
		DataOutputStream dos 		= new DataOutputStream(baos);
		
		try {
			zObject.writeDataStream(dos);
			dos.flush();
			dos.close();
			baos.close();
		
			return new MiniData(baos.toByteArray());
			
		} catch (IOException e) {
			MinimaLogger.log(e);	
		}
		
		return null;
	}
	
	/**
	 * Get a random chunk of data
	 * 
	 * @param len
	 * @return
	 */
	public static MiniData getRandomData(int len) {
		SecureRandom rand = new SecureRandom();
		byte[] data = new byte[len];
		rand.nextBytes(data);
		return new MiniData(data);
	}
	
	public static void main(String[] zArgs) {
		
		MiniData origdata = MiniData.getRandomData(10);
		System.out.println("ORIG   : "+origdata.getLength()+" "+origdata.to0xString() +" "+origdata.getDataValue());
		
		MiniData lendata = new MiniData(origdata.getBytes(), 16);
		System.out.println("LENGTH : "+lendata.getLength()+" "+lendata.to0xString() +" "+lendata.getDataValue());
		
		
	}
}
