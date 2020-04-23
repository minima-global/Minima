package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import org.minima.utils.Streamable;


/**
 * VERY Simple VERY Accurate Real Number class that only does addition for the MMRSum Tree..
 * 
 * Higher precision than MiniNumber
 * 
 * @author spartacusrex
 *
 */
public class MMRSumNumber implements Streamable {
	
	private static final MathContext mMathContext = new MathContext(128, RoundingMode.DOWN);
	
	public static final MMRSumNumber ZERO = new MMRSumNumber(BigDecimal.ZERO);
	
	/**
	 * The number representation
	 */
	private BigDecimal mNumber;
	
	private MMRSumNumber() {}
	
	private MMRSumNumber(BigDecimal zNumber) {
		mNumber = zNumber;
	}
	
	public MMRSumNumber(MiniNumber zNumber) {
		mNumber = new BigDecimal(zNumber.toString(), mMathContext);
	}
	
	public BigDecimal getNumber() {
		return mNumber;
	}
	
	public MMRSumNumber add(MMRSumNumber zNumber) {
		return new MMRSumNumber(mNumber.add(zNumber.getNumber(), mMathContext));
	}
	
	public boolean isEqual(MMRSumNumber zNumber) {
		return zNumber.getNumber().compareTo(mNumber) == 0;
	}
	
	@Override
	public String toString() {
		return mNumber.stripTrailingZeros().toPlainString();
	}
	
	/**
	 * Output the scale and unscaled value..
	 */
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write out the scale..
		zOut.writeInt(mNumber.scale());
		
		//And now the unscaled value..
		byte[] data = mNumber.unscaledValue().toByteArray();
		int len = data.length;
		
		zOut.writeInt(len);
		zOut.write(data);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//Read in the scale
		int scale = zIn.readInt();
		
		//Read in the byte array for unscaled BigInteger
		int len = zIn.readInt();
		byte[] data = new byte[len];
		zIn.readFully(data);
		
		//And create..
		BigInteger unscaled = new BigInteger(data);
		mNumber = new BigDecimal(unscaled,scale,mMathContext);
	}

	public static MMRSumNumber ReadFromStream(DataInputStream zIn){
		MMRSumNumber data = new MMRSumNumber();
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
}
