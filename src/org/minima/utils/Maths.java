/**
 * 
 */
package org.minima.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.TimeUnit;

/**
 * @author Spartacus Rex
 *
 */
public class Maths {
	
	public static final BigInteger BI_TWO = new BigInteger("2");
	public static final BigDecimal BD_TWO = new BigDecimal("2");
	

	public static double log2(double zDD){
		return Math.log10(zDD) / Math.log10(2);
	}
	
//	public static String getDataAsString(byte[] zData){
//		return "0x"+new BigInteger(1,zData).toString(16).toUpperCase();
//	}
	
	public static double log2BI(BigInteger val)
	{
	    // Get the minimum number of bits necessary to hold this value.
	    int n = val.bitLength();

	    // Calculate the double-precision fraction of this number; as if the
	    // binary point was left of the most significant '1' bit.
	    // (Get the most significant 53 bits and divide by 2^53)
	    long mask = 1L << 52; // mantissa is 53 bits (including hidden bit)
	    long mantissa = 0;
	    int j = 0;
	    for (int i = 1; i < 54; i++)
	    {
	        j = n - i;
	        if (j < 0) break;

	        if (val.testBit(j)) mantissa |= mask;
	        mask >>>= 1;
	    }
	    // Round up if next bit is 1.
	    if (j > 0 && val.testBit(j - 1)) mantissa++;

	    double f = mantissa / (double)(1L << 52);

	    // Add the logarithm to the number of bits, and subtract 1 because the
	    // number of bits is always higher than necessary for a number
	    // (ie. log2(val)<n for every val).
	    return (n - 1 + Math.log(f) * 1.44269504088896340735992468100189213742664595415298D);
	    // Magic number converts from base e to base 2 before adding. For other
	    // bases, correct the result, NOT this number!
	}
	
	public static String ConvertMilliToTime(long zMilli) {
		
		long milliseconds = zMilli;
		
		long dy = TimeUnit.MILLISECONDS.toDays(milliseconds);
		
		long yr = dy / 365;
		dy %= 365;
		
		long mn = dy / 30;
		dy %= 30;
		
		long wk = dy / 7;
		dy %= 7;
		
		long hr = TimeUnit.MILLISECONDS.toHours(milliseconds)
				- TimeUnit.DAYS.toHours(TimeUnit.MILLISECONDS.toDays(milliseconds));
		
		long min = TimeUnit.MILLISECONDS.toMinutes(milliseconds)
				- TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(milliseconds));
		
		long sec = TimeUnit.MILLISECONDS.toSeconds(milliseconds)
				- TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(milliseconds));
		
		long ms = TimeUnit.MILLISECONDS.toMillis(milliseconds)
				- TimeUnit.SECONDS.toMillis(TimeUnit.MILLISECONDS.toSeconds(milliseconds));
		
		return String.format("%d Years %d Months %d Weeks %d Days %d Hours %d Minutes %d Seconds %d Milliseconds", 
						yr,mn, wk, dy, hr, min, sec, ms);
	}	
	
}
