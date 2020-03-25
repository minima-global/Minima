package org.minima.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniHash;

public class SuperBlockLevels {

//	public static final int MAX_CASCADE_LEVELS = 32;
	
//	private static SuperBlockLevels mSuperDuper = null;
//	public static SuperBlockLevels getSupers() {
//		if(mSuperDuper == null) {
//			mSuperDuper = new SuperBlockLevels();
//		}
//		return mSuperDuper;
//	}
//
//
//	public static final int MAX_LEVELS = 32;
//	
//	BigInteger[] mSuperLevels = new BigInteger[MAX_LEVELS];
//	
//	BigDecimal[] mPOW2 = new BigDecimal[MAX_LEVELS];
//	
//	public SuperBlockLevels() {
//		BigInteger two 		= new BigInteger("2");
//		BigDecimal twodec 	= new BigDecimal("2");
//		
//		for(int i=0;i<MAX_LEVELS;i++) {
//			//Create a Big number
////			mSuperLevels[MAX_LEVELS-1-i] =  two.pow(i);
//			
//			mSuperLevels[i] =  two.pow(256-i);
//			
//			mPOW2[i] = twodec.pow(i);
//		}
//	}
//	
//	public BigDecimal get2POW(int zExponent) {
//		return mPOW2[zExponent];
//	}
//	
//	public int getSuperBlockLevel(MiniHash zData) {
//		for(int i=MAX_LEVELS-1;i>=0;i--) {
//			String hex = zData.toPureHexString();
//			
//			BigInteger val = new BigInteger(hex, 16);
//		
//			if(val.compareTo(mSuperLevels[i]) <= 0) {
//				return i;
//			}
//		}
//		
//		return -1;
//	}
//	
//	public BigInteger getSuperValue(int zLevel) {
//		return mSuperLevels[zLevel];
//	}

	/**
	 * Get the Cascade Level of this Hash Value.. in comparison to what it needed to be
	 * @param zDifficulty
	 * @param zActual
	 * @return
	 */
	public static int getSuperLevel(MiniHash zDifficulty, MiniHash zActual) {
		
		BigDecimal bigdiff = new BigDecimal(zDifficulty.getDataValue(), MathContext.DECIMAL128);
		BigDecimal bigact  = new BigDecimal(zActual.getDataValue(), MathContext.DECIMAL128);
		
		//Divide the 2..
		BigDecimal sup = bigdiff.divide(bigact, MathContext.DECIMAL128);
		
		//Get as a BigInteger..
		BigInteger supbig = sup.toBigInteger();
		
		int ll2 = (int) Maths.log2BI(supbig);
		
		if(ll2 > GlobalParams.MINIMA_CASCADE_LEVELS-1) {
			ll2 = GlobalParams.MINIMA_CASCADE_LEVELS-1;
		}
//		int ll2 = (int)Maths.log2(sup.doubleValue());
		
		return ll2;
	}
	
}
