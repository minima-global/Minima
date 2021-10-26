package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class MMREntryNumber implements Streamable, Comparable<MMREntryNumber> {

	/** 
	 * The base Math Context used for all operations
	 */
	public static final MathContext MMR_MATH_CONTEXT = MathContext.UNLIMITED;
	
	public static final MMREntryNumber ZERO 	= new MMREntryNumber(BigDecimal.ZERO);
	public static final MMREntryNumber TWO 	= new MMREntryNumber(new BigDecimal(2));
	
	/**
	 * The number representation
	 */
	private BigDecimal mNumber;
	
	public MMREntryNumber() {
		mNumber = BigDecimal.ZERO;
	}
	
	public MMREntryNumber(int zNumber) {
		mNumber = new BigDecimal(zNumber, MMR_MATH_CONTEXT);
	}
	
	public MMREntryNumber(BigDecimal zBigDecimal) {
		mNumber = zBigDecimal;
	}
	
	public MMREntryNumber(BigInteger zBigInteger) {
		mNumber = new BigDecimal(zBigInteger, MMR_MATH_CONTEXT);
	}
	
	public BigDecimal getBigDecimal() {
		return mNumber;
	}
	
	public MMREntryNumber modulo(MMREntryNumber zNumber) {
		return new MMREntryNumber( mNumber.remainder(zNumber.getBigDecimal(),MMR_MATH_CONTEXT) );
	}
	
	public MMREntryNumber floor() {
		return new MMREntryNumber( mNumber.setScale(0, RoundingMode.FLOOR) ) ;
	}
	
	public MMREntryNumber increment() {
		return new MMREntryNumber( mNumber.add(BigDecimal.ONE,MMR_MATH_CONTEXT) );
	}
	
	public MMREntryNumber decrement() {
		return new MMREntryNumber( mNumber.subtract(BigDecimal.ONE,MMR_MATH_CONTEXT) );
	}
	
	public MMREntryNumber div2() {
		return new MMREntryNumber( mNumber.divide(TWO.getBigDecimal(), MMR_MATH_CONTEXT) );
	}
	
	public MMREntryNumber mult2() {
		return new MMREntryNumber( mNumber.multiply(TWO.getBigDecimal(),MMR_MATH_CONTEXT) );
	}
	
	@Override
	public int compareTo(MMREntryNumber zCompare) {
		return mNumber.compareTo(zCompare.getBigDecimal());
	}
	
	public boolean isEqual(MMREntryNumber zNumber) {
		return compareTo(zNumber)==0;
	}
	
	public boolean isLess(MMREntryNumber zNumber) {
		return compareTo(zNumber)<0;
	}
	
	public boolean isLessEqual(MMREntryNumber zNumber) {
		return compareTo(zNumber)<=0;
	}
	
	public boolean isMore(MMREntryNumber zNumber) {
		return compareTo(zNumber)>0;
	}
	
	public boolean isMoreEqual(MMREntryNumber zNumber) {
		return compareTo(zNumber)>=0;
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
	
		//Write the scale
		MiniNumber.WriteToStream(zOut, mNumber.scale());
		
		//Write the unscaled value
		MiniData.WriteToStream(zOut, mNumber.unscaledValue().toByteArray());
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		//Scale
		int scale = MiniNumber.ReadFromStream(zIn).getAsInt();
		
		//Length of unscaled value
		MiniData unscaleddata = MiniData.ReadFromStream(zIn);
		
		//Create the number
		mNumber = new BigDecimal(new BigInteger(unscaleddata.getBytes()), scale, MMR_MATH_CONTEXT);
	}
	
	public static MMREntryNumber ReadFromStream(DataInputStream zIn) throws IOException{
		MMREntryNumber data = new MMREntryNumber();
		data.readDataStream(zIn);
		return data;
	}
}
