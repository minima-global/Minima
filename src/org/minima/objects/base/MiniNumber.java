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

import org.minima.utils.Crypto;
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
	public static final int MAX_DIGITS = 34;
	
	/**
	 * The Maximum number of decimal places for a Minima Value
	 * 
	 * 10 digits represents 1 billion ( Max Value )..
	 */
	public static final int MAX_MINIMA_DECIMAL_PLACES = MAX_DIGITS - 10;
	
	/**
	 * Max Decimal Places for any MiniNumber
	 */
	public static final int MAX_DECIMAL_PLACES = 128;
	
	/** 
	 * The base Math Context used for all operations
	 */
	public static final MathContext MATH_CONTEXT = new MathContext(MAX_DIGITS, RoundingMode.DOWN);
	
	/**
	 * The MAXIMUM value any MiniNumber can be..
	 */
	public static final BigDecimal MAX_MININUMBER = new BigDecimal(10,MATH_CONTEXT).pow(512,MATH_CONTEXT);
	
	/**
	 * The Minimum value any MiniNumber can be..
	 */
	public static final BigDecimal MIN_MININUMBER = MAX_MININUMBER.negate();
	
	/**
	 * Useful numbers
	 */
	public static final MiniNumber ZERO 		= new MiniNumber("0");
	public static final MiniNumber ONE 		    = new MiniNumber("1");
	public static final MiniNumber TWO 		    = new MiniNumber("2");
	public static final MiniNumber FOUR 		= new MiniNumber("4");
	public static final MiniNumber EIGHT        = new MiniNumber("8");
	public static final MiniNumber TWELVE       = new MiniNumber("12");
	public static final MiniNumber SIXTEEN      = new MiniNumber("16");
	public static final MiniNumber THIRTYTWO    = new MiniNumber("32");
	public static final MiniNumber SIXTYFOUR    = new MiniNumber("64");
	public static final MiniNumber TWOFIVESIX   = new MiniNumber("256");
	public static final MiniNumber FIVEONE12    = new MiniNumber("512");
	public static final MiniNumber THOUSAND24   = new MiniNumber("1024");
	
	public static final MiniNumber TEN          = new MiniNumber("10");
	public static final MiniNumber HUNDRED      = new MiniNumber("100");
	public static final MiniNumber THOUSAND     = new MiniNumber("1000");
	public static final MiniNumber MILLION      = new MiniNumber("1000000");
	public static final MiniNumber BILLION      = new MiniNumber("1000000000");
	
	public static final MiniNumber MINUSONE 	= new MiniNumber("-1");
	
	public static final MiniNumber POINTONE 	= new MiniNumber("0.1");
	public static final MiniNumber ONEPOINTONE 	= new MiniNumber("1.1");
	public static final MiniNumber POINTNINE 	= new MiniNumber("0.9");
	
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
	
	public MiniNumber(String zNumber){
		mNumber = new BigDecimal(zNumber,MATH_CONTEXT);
		checkLimits();
	}
	
	public MiniNumber(BigDecimal zBigD){
		mNumber = zBigD.round(MATH_CONTEXT);
		checkLimits();
	}
	
	/**
	 * Check MiniNumber is within the acceptable range
	 */
	private void checkLimits() {
		if(mNumber.compareTo(MAX_MININUMBER)>0) {
			throw new NumberFormatException("MiniNumber too large - outside allowed range 10^512");
		}
		
		if(mNumber.compareTo(MIN_MININUMBER)<0) {
			throw new NumberFormatException("MiniNumber too small - outside allowed range -(10^512)");
		}
		
		if(mNumber.scale() > MAX_DECIMAL_PLACES) {
			throw new NumberFormatException("MiniNumber too many decimal places");
		}
	}
	
	/**
	 * Make a VALID Minima number.. within the allowed range and decimals
	 * @return
	 */
	public MiniNumber getAsMinimaValue() {
		if(isMore(BILLION)) {
			return BILLION;
		}
		
		if(isLess(ZERO)) {
			return ZERO;
		}
		
		return new MiniNumber(mNumber.setScale(MAX_MINIMA_DECIMAL_PLACES, RoundingMode.DOWN));
	}
	
	/**
	 * Is this a valid number for an input or an output in Minima
	 * @return true false..
	 */
	public boolean isValidMinimaValue() {
		return isEqual(getAsMinimaValue());
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
	
	public double getAsDouble() {
		return mNumber.doubleValue();
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
	
	public MiniNumber divRoundDown(MiniNumber zNumber) {
		return new MiniNumber( mNumber.divide(zNumber.getAsBigDecimal(), RoundingMode.DOWN) );
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
			sigdig = MAX_DIGITS;	
		}else if(sigdig<1) {
			sigdig = 1;
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
		if(len > 128 || len<1) {
			//Something wrong..
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
	
	public static void main(String[] zargs) {
		MiniNumber num1 = new MiniNumber("100000000");
		System.out.println(num1 + " valid:"+num1.isValidMinimaValue());
		
		MiniNumber num2 = new MiniNumber("0.0000000001");
		System.out.println(num2+" "+num2.decimalPlaces());
	
		MiniNumber num3 = new MiniNumber("0.00000000000001");
		System.out.println(num3 + " "+num3.getAsMinimaValue()+" valid:"+num3.isValidMinimaValue());
		
		MiniNumber num4 = new MiniNumber("1.23E+04");
		System.out.println(num4);
		
		//MiniNumber num5 = new MiniNumber(new MiniData("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF").getDataValue());
		MiniNumber num5 = new MiniNumber(Crypto.MAX_HASH.getDataValue());
		System.out.println(num5);
		
		
	}
	
	
}
