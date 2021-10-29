/**
 * 
 */
package org.minima.utils;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.system.params.GlobalParams;

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
	
	/**
	 * Get the Cascade Level of this Hash Value.. in comparison to what it needed to be
	 * @param zDifficulty
	 * @param zActual
	 * @return
	 */
	public static int getSuperLevel(MiniData zDifficulty, MiniData zActual) {
		BigInteger supbig = zDifficulty.getDataValue().divide(zActual.getDataValue());
		
		int ll2 = (int) Maths.log2BI(supbig);
		if(ll2<0) {
			ll2 = 0;
		}
		
//		//Now do a check to see if it's half way or not..
//		BigInteger low  = zDifficulty.getDataValue().multiply(TWO.pow(ll2));
//		BigInteger high = zDifficulty.getDataValue().multiply(TWO.pow(ll2+1));
//		BigInteger mid  = low.add(high).divide(TWO);
//		if(zActual.getDataValue().compareTo(mid)>0) {
//			ll2++;
//		}
		
		if(ll2 > GlobalParams.MINIMA_CASCADE_LEVELS-1) {
			ll2 = GlobalParams.MINIMA_CASCADE_LEVELS-1;
		}
		
		return ll2;
	}
	
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
	
	public static void main(String[] zArgs) {
		
		BigInteger size = new BigInteger("255");
		
		MiniData diff 	= new MiniData(size);
		
		for(int i=1;i<=size.intValue();i++) {
			BigInteger val  = new BigInteger(""+i);
			MiniData txpow 	= new MiniData(val);
		
			BigInteger supbig = diff.getDataValue().divide(txpow.getDataValue());
			int tester = supbig.bitLength()-1;
			int myway = (int) Maths.log2BI(supbig);
			
//			if(tester != myway) {
				System.out.println(txpow.to0xString()+" new:"+tester+" old:"+myway);
//			}
		}
	}
	
}
