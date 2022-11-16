/**
 * 
 */
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
 * All the Inputs and Outputs of transactions use this class.
 * 
 * Too high precision would be too slow.
 * 
 * @author Spartacus Rex
 *
 */
public class MiniNumber implements Streamable, Comparable<MiniNumber> {
	
	/**
	 * The MAX Number of Significant digits for any MiniNUmber
	 */
	public static final int MAX_DIGITS = 64;
	
	/**
	 * MAX number is 8 byte unsigned long.. 2^64 -1 .. 20 digits
	 */
	public static final int MAX_DECIMAL_PLACES = MAX_DIGITS - 20;
	
	/** 
	 * The base Math Context used for all operations
	 */
	public static final MathContext MATH_CONTEXT = new MathContext(MAX_DIGITS, RoundingMode.DOWN);
	
	/**
	 * The MAXIMUM value any MiniNumber can be..
	 * 
	 * 2^64 - 1 or as HEX 0xFFFFFFFFFFFFFFFF
	 */
	public static final BigDecimal MAX_MININUMBER = new BigDecimal(2).pow(64).subtract(BigDecimal.ONE);
	
	/**
	 * The Minimum value any MiniNumber can be..
	 */
	public static final BigDecimal MIN_MININUMBER = MAX_MININUMBER.negate();
	
	/**
	 * The smallest unit possible
	 */
	public static final MiniNumber MINI_UNIT = new MiniNumber("1E-"+MAX_DECIMAL_PLACES);

	/**
	 * The Maximum possible MiniNumber
	 */
	public static final MiniNumber MAXIMUM 	 = new MiniNumber(MAX_MININUMBER);
	
	/**
	 * Useful numbers
	 */
	public static final MiniNumber ZERO 		= new MiniNumber("0");
	public static final MiniNumber ONE 		    = new MiniNumber("1");
	public static final MiniNumber TWO 		    = new MiniNumber("2");
	public static final MiniNumber THREE 		= new MiniNumber("3");
	public static final MiniNumber FOUR 		= new MiniNumber("4");
	public static final MiniNumber EIGHT        = new MiniNumber("8");
	public static final MiniNumber TWELVE       = new MiniNumber("12");
	public static final MiniNumber SIXTEEN      = new MiniNumber("16");
	public static final MiniNumber TWENTY      	= new MiniNumber("20");
	public static final MiniNumber THIRTYTWO    = new MiniNumber("32");
	public static final MiniNumber FIFTY    	= new MiniNumber("50");
	public static final MiniNumber SIXTYFOUR    = new MiniNumber("64");
	public static final MiniNumber TWOFIVESIX   = new MiniNumber("256");
	public static final MiniNumber FIVEONE12    = new MiniNumber("512");
	public static final MiniNumber THOUSAND24   = new MiniNumber("1024");
	
	public static final MiniNumber TEN          = new MiniNumber("1E1");
	public static final MiniNumber HUNDRED      = new MiniNumber("1E2");
	public static final MiniNumber THOUSAND     = new MiniNumber("1E3");
	public static final MiniNumber MILLION      = new MiniNumber("1E6");
	public static final MiniNumber HUNDMILLION  = new MiniNumber("1E8");
	public static final MiniNumber BILLION      = new MiniNumber("1E9");
	public static final MiniNumber TRILLION     = new MiniNumber("1E12");
	
	public static final MiniNumber MINUSONE 	= new MiniNumber("-1");
	
	/**
	 * The number representation
	 */
	private BigDecimal mNumber;
	
	/**
	 * Many different COnstructors for all number types
	 */
	public MiniNumber(){
		mNumber = new BigDecimal(0,MATH_CONTEXT);
	}
	
	public MiniNumber(int zNumber){
		mNumber = new BigDecimal(zNumber,MATH_CONTEXT);
		checkLimits();
	}
	
	public MiniNumber(long zNumber){
		mNumber = new BigDecimal(zNumber,MATH_CONTEXT);
		checkLimits();
	}

	public MiniNumber(BigInteger zNumber){
		mNumber = new BigDecimal(zNumber,MATH_CONTEXT);
		checkLimits();
	}
	
	public MiniNumber(BigDecimal zNumber){
		mNumber = new BigDecimal(zNumber.toPlainString(),MATH_CONTEXT);
		checkLimits();
	}
	
	public MiniNumber(MiniNumber zMiniNumber){
		mNumber = zMiniNumber.getAsBigDecimal();
		checkLimits();
	}
	
	public MiniNumber(String zNumber){
		mNumber = new BigDecimal(zNumber,MATH_CONTEXT);
		checkLimits();
	}
	
	public BigDecimal getNumber() {
		return getAsBigDecimal();
	}
	
	/**
	 * Check MiniNumber is within the acceptable range
	 */
	private void checkLimits() {
		if(mNumber.scale() > MAX_DECIMAL_PLACES) {
			mNumber = mNumber.setScale(MAX_DECIMAL_PLACES, RoundingMode.DOWN);
		}
		
		if(mNumber.compareTo(MAX_MININUMBER)>0) {
			throw new NumberFormatException("MiniNumber too large - outside allowed range 2^64 "+mNumber);
		}
		
		if(mNumber.compareTo(MIN_MININUMBER)<0) {
			throw new NumberFormatException("MiniNumber too small - outside allowed range -(2^64)");
		}
	}
	
	/**
	 * Is this a valid number for an input or an output in Minima
	 * @return true false..
	 */
	public boolean isValidMinimaValue() {
		return isLessEqual(MiniNumber.BILLION) && isMore(MiniNumber.ZERO);
	}
	
	/**
	 * Convert to various normal number types
	 */
	public BigDecimal getAsBigDecimal() {
		return mNumber;
	}
	
	public BigInteger getAsBigInteger() {
		return mNumber.toBigInteger();
	}
	
	public long getAsLong() {
		return mNumber.longValue();
	}
	
	public int getAsInt() {
		return mNumber.intValue();
	}
	
	/**
	 * Basic arithmetic functions 
	 */
	public MiniNumber add(MiniNumber zNumber) {
		return new MiniNumber( mNumber.add(zNumber.getAsBigDecimal(),MATH_CONTEXT) );
	}
	
	public MiniNumber sub(MiniNumber zNumber) {
		return new MiniNumber( mNumber.subtract(zNumber.getAsBigDecimal(),MATH_CONTEXT) );
	}
	
	public MiniNumber div(MiniNumber zNumber) {
		return new MiniNumber( mNumber.divide(zNumber.getAsBigDecimal(), MATH_CONTEXT) );
	}
	
	public MiniNumber mult(MiniNumber zNumber) {
		return new MiniNumber( mNumber.multiply(zNumber.getAsBigDecimal(),MATH_CONTEXT) );
	}
	
	public MiniNumber pow(int zNumber) {
		return new MiniNumber( mNumber.pow(zNumber,MATH_CONTEXT) );
	}
	
	public MiniNumber modulo(MiniNumber zNumber) {
		return new MiniNumber( mNumber.remainder(zNumber.getAsBigDecimal(),MATH_CONTEXT) );
	}
	
	public MiniNumber floor() {
		return new MiniNumber( mNumber.setScale(0, RoundingMode.FLOOR) ) ;
	}
	
	public MiniNumber ceil() {
		return new MiniNumber( mNumber.setScale(0, RoundingMode.CEILING) ) ;
	}
	
	public MiniNumber setSignificantDigits(int zSignificantDigits) {
		//1-max digits..
		int sigdig = zSignificantDigits;
		if(sigdig>MAX_DIGITS) {
			throw new NumberFormatException("Cannot specify this many significant digits "+sigdig);	
		}else if(sigdig<0) {
			throw new NumberFormatException("Cannot specify negative significant digits "+sigdig);
		}
		
		return new MiniNumber( mNumber.round(new MathContext(sigdig, RoundingMode.DOWN))) ;
	}
	
	public MiniNumber abs() {
		return new MiniNumber( mNumber.abs() ) ;
	}
		
	public MiniNumber increment() {
		return new MiniNumber( mNumber.add(BigDecimal.ONE,MATH_CONTEXT) );
	}
	
	public MiniNumber decrement() {
		return new MiniNumber( mNumber.subtract(BigDecimal.ONE,MATH_CONTEXT) );
	}

	public int decimalPlaces() {
		return mNumber.scale();
	}
	
	@Override
	public int compareTo(MiniNumber zCompare) {
		return mNumber.compareTo(zCompare.getAsBigDecimal());
	}
	
	public boolean isEqual(MiniNumber zNumber) {
		return compareTo(zNumber)==0;
	}
	
	public boolean isLess(MiniNumber zNumber) {
		return compareTo(zNumber)<0;
	}
	
	public boolean isLessEqual(MiniNumber zNumber) {
		return compareTo(zNumber)<=0;
	}
	
	public boolean isMore(MiniNumber zNumber) {
		return compareTo(zNumber)>0;
	}
	
	public boolean isMoreEqual(MiniNumber zNumber) {
		return compareTo(zNumber)>=0;
	}
		
	@Override
	public String toString(){
		return mNumber.stripTrailingZeros().toPlainString();
	}

	/**
	 * Output the scale and unscaled value..
	 */
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		//Write out the scale.. +/-127 - never more than 1 byte in size though  
		int scale = mNumber.scale();
		zOut.writeByte(scale);
		
		//And now the unscaled value.. never larger than..29
		byte[] data = mNumber.unscaledValue().toByteArray();
		zOut.writeByte(data.length);
		
		//WRITE THE DATA
		zOut.write(data);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		//Read in the scale
		int scale = (int)zIn.readByte();
		
		//Read in the byte array for unscaled BigInteger
		int len = (int)zIn.readByte();
		if(len > 32 || len<0) {
			throw new IOException("ERROR reading MiniNumber - input too large or negative "+len);
		}
			
		byte[] data = new byte[len];
		zIn.readFully(data);
		
		//And create..
		BigInteger unscaled = new BigInteger(data);
		mNumber = new BigDecimal(unscaled,scale,MATH_CONTEXT);
	}
	
	public static MiniNumber ReadFromStream(DataInputStream zIn) throws IOException{
		MiniNumber data = new MiniNumber();
		data.readDataStream(zIn);
		return data;
	}
	
	public static void WriteToStream(DataOutputStream zOut, int zNumber) throws IOException{
		new MiniNumber(zNumber).writeDataStream(zOut);
	}
}
