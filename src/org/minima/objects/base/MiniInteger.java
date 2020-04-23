package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.minima.utils.Streamable;

/**
 * NO limit Very Large whole numbers for the MMR Entry..
 * @author spartacusrex
 *
 */
public class MiniInteger implements Streamable {

	public static MiniInteger ZERO = new MiniInteger(0);
	public static MiniInteger ONE  = new MiniInteger(1);
	public static MiniInteger TWO  = new MiniInteger(2);
		
	private BigInteger mNumber;
	
	private MiniInteger() {}
	
	public MiniInteger(BigInteger zNumber) {
		mNumber = zNumber;
	}
	
	public MiniInteger(int zNumber) {
		mNumber = new BigInteger(Integer.toString(zNumber));
	}
	
	public BigInteger getNumber() {
		return mNumber;
	}
	
	public boolean isEqual(MiniInteger zNumber) {
		return mNumber.compareTo(zNumber.getNumber()) == 0;
	}
	
	public boolean isLess(MiniInteger zNumber) {
		return mNumber.compareTo(zNumber.getNumber()) < 0;
	}
	
//	public boolean isLessEqual(MiniInteger zNumber) {
//		return mNumber.compareTo(zNumber.getNumber()) <= 0;
//	}
//	
//	public boolean isMore(MiniInteger zNumber) {
//		return mNumber.compareTo(zNumber.getNumber()) > 0;
//	}
//
//	public boolean isMoreEqual(MiniInteger zNumber) {
//		return mNumber.compareTo(zNumber.getNumber()) >= 0;
//	}
	
	public MiniInteger modulo(MiniInteger zNumber) {
		return new MiniInteger(mNumber.mod(zNumber.getNumber()));
	}
	
	public MiniInteger add(MiniInteger zNumber) {
		return new MiniInteger(mNumber.add(zNumber.getNumber()));
	}
	
	public MiniInteger sub(MiniInteger zNumber) {
		return new MiniInteger(mNumber.subtract(zNumber.getNumber()));
	}
	
	public MiniInteger divRoundDown(MiniInteger zNumber) {
		BigDecimal bigd    = new BigDecimal(mNumber);
		BigDecimal bigddiv = new BigDecimal(zNumber.getNumber());
		BigDecimal ans     = bigd.divide(bigddiv, RoundingMode.DOWN);
		
		return new MiniInteger(ans.toBigInteger());
	}
	
	public MiniInteger mult(MiniInteger zNumber) {
		return new MiniInteger(mNumber.multiply(zNumber.getNumber()));
	}
	
	public MiniInteger increment() {
		return add(ONE);
	}
	
	@Override
	public String toString() {
		return mNumber.toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		byte[] bytes = mNumber.toByteArray();
		int len      = bytes.length;
		
		zOut.writeInt(len);
		zOut.write(bytes);	
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//Read in the byte array for unscaled BigInteger
		int len = zIn.readInt();
		byte[] data = new byte[len];
		zIn.readFully(data);
		
		mNumber = new BigInteger(data);
	}
	
	public static MiniInteger ReadFromStream(DataInputStream zIn) {
	   MiniInteger data = new MiniInteger();
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			return null;
		}
		
		return data;
	}
}
