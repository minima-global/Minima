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

import org.minima.system.network.NetClientReader;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;

/**
 * @author Spartacus Rex
 *
 */
public class MiniData implements Streamable {
	
	/**
	 * The byte data
	 */
	protected byte[] mData;
	
	/**
	 * The numeric value of the data
	 */
	protected BigInteger mDataVal;
	
	public MiniData() {
		this(new byte[0]);
	}
	
	public MiniData(String zHexString) {
		this(BaseConverter.decode16(zHexString));
	}
	
	public MiniData(byte[] zData) {
		mData = zData;
		setDataValue();
	}
	
	private void setDataValue() {
		mDataVal = new BigInteger(1,mData);
	}
	
	public int getLength() {
		return mData.length;
	}
	
	public byte[] getData() {
		return mData;
	}
	
	public BigInteger getDataValue() {
		return mDataVal;
	}
	
	public BigDecimal getDataValueDecimal() {
		return new BigDecimal(mDataVal);
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
		byte[] data = zCompare.getData();
		
		//Check the data..
		for(int i=0;i<len;i++) {
			if(data[i] != mData[i]) {
				return false;
			}
		}
	
		return true;
//		return mDataVal.compareTo(zCompare.getDataValue()) == 0;
	}
	
	public boolean isLess(MiniData zCompare) {
		return mDataVal.compareTo(zCompare.getDataValue()) < 0;
	}
	
	public boolean isLessEqual(MiniData zCompare) {
		return mDataVal.compareTo(zCompare.getDataValue()) <= 0;
	}
	
	public boolean isMore(MiniData zCompare) {
		return mDataVal.compareTo(zCompare.getDataValue()) > 0;
	}
	
	public boolean isMoreEqual(MiniData zCompare) {
		return mDataVal.compareTo(zCompare.getDataValue()) >= 0;
	}
	
	public MiniData shiftr(int zNumber) {
		return new MiniData(mDataVal.shiftRight(zNumber).toString(16).toUpperCase());
	}
	
	public MiniData shiftl(int zNumber) {
		return new MiniData(mDataVal.shiftLeft(zNumber).toString(16).toUpperCase());
	}
	
	public int compare(MiniData zCompare) {
		return mDataVal.compareTo(zCompare.getDataValue());
	}
	
	public MiniData concat(MiniData zConcat) {
		int locallen  = getLength();
		int concatlen = zConcat.getData().length; 
				
		int totlen   = locallen+concatlen;
		byte[] total = new byte[totlen]; 
		
		//First copy local..
		System.arraycopy(getData(), 0, total, 0, locallen);
		
		//Then the new..
		System.arraycopy(zConcat.getData(), 0, total, locallen, concatlen);
		
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
		if(len > NetClientReader.MAX_INTRO) {
			throw new IOException("Read Error : MiniData Length larger than maximum allowed "+len);
		}
		
		if(zSize != -1) {
			if(len != zSize) {
				throw new IOException("Read Error : MiniData Length not correct as specified "+zSize);
			}
		}
		mData = new byte[len];
		zIn.readFully(mData);
		
		//Set the data value
		setDataValue();
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
		if(mData.length > Crypto.MINIMA_DEFAULT_MAX_HASH_LENGTH) {
			throw new IOException("Write Error : HASH Length greater than 64! "+mData.length);
		}
		
		zOut.writeInt(mData.length);
		zOut.write(mData);
	}

	public void readHashFromStream(DataInputStream zIn) throws IOException {
		int len = zIn.readInt();
		if(len > Crypto.MINIMA_DEFAULT_MAX_HASH_LENGTH) {
			throw new IOException("Read Error : HASH Length greater then 64! "+len);
		}
		
		mData = new byte[len];
		zIn.readFully(mData);
		
		//Set the data value
		setDataValue();
	}
	
	public static MiniData ReadHashFromStream(DataInputStream zIn) throws IOException{
		MiniData data = new MiniData();
		data.readHashFromStream(zIn);
		return data;
	}
	
	public static MiniData getMiniDataVersion(Streamable zObject) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		
		try {
			zObject.writeDataStream(dos);
			dos.flush();
		} catch (IOException e) {
			return null;
		}
		
		return new MiniData(baos.toByteArray());
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
		
		MiniData data1 = getRandomData(128);
		MiniData data2 = getRandomData(128);
		
		long timenow = System.currentTimeMillis();
		boolean allsame = true;
		for(int i=0;i<10000000;i++) {
			boolean same = data1.isEqual(data2);
			if(!same) {
				allsame = false;
			}
		}
		
		long timediff = System.currentTimeMillis() - timenow;
		System.out.println(allsame+" "+timediff);
		
	}
}
