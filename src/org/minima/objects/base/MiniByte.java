package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.utils.Streamable;

public class MiniByte implements Streamable{

	/**
	 * Global True False
	 */
	public static final MiniByte TRUE  = new MiniByte(1);
	public static final MiniByte FALSE = new MiniByte(0);
	
	byte mVal;
	
	public MiniByte() {
		mVal = 0;
	}
	
	public MiniByte(int val) {
		mVal = (byte)val;
	}
	
	public MiniByte(byte zVal) {
		mVal = zVal;
	}
	
	public MiniByte(boolean zVal) {
		if(zVal) {
			mVal = 1;
		}else {
			mVal = 0;
		}
	}

	public int getValue() {
		return ( mVal & 0xFF );
	}
	
	public byte getByteValue() {
		return mVal;
	}
	
	public boolean isEqual(MiniByte zRamByte) {
		return getValue() == zRamByte.getValue();
	}
	
	public boolean isFalse() {
		return isEqual(FALSE);
	}
	
	public boolean isTrue() {
		return !isFalse();
	}
	
	@Override
	public String toString() {
		return ""+getValue();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeByte(mVal);	
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVal = zIn.readByte();
	}
	
	public static MiniByte ReadFromStream(DataInputStream zIn){
		MiniByte data = new MiniByte();
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
//			e.printStackTrace();
			return null;
		}
		
		return data;
	}
}
