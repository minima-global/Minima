package org.minima.utils;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.minima.objects.base.MiniHash;

public class SuperBlockLevels {

	private static SuperBlockLevels mSuperDuper = null;
	public static SuperBlockLevels getSupers() {
		if(mSuperDuper == null) {
			mSuperDuper = new SuperBlockLevels();
		}
		return mSuperDuper;
	}


	public static final int MAX_LEVELS = 256;
	
	BigInteger[] mSuperLevels = new BigInteger[MAX_LEVELS];
	
	BigDecimal[] mPOW2 = new BigDecimal[MAX_LEVELS];
	
	public SuperBlockLevels() {
		BigInteger two 		= new BigInteger("2");
		BigDecimal twodec 	= new BigDecimal("2");
		
		for(int i=0;i<MAX_LEVELS;i++) {
			//Create a Big number
//			mSuperLevels[MAX_LEVELS-1-i] =  two.pow(i);
			
			mSuperLevels[i] =  two.pow(256-i);
			
			mPOW2[i] = twodec.pow(i);
		}
	}
	
	public BigDecimal get2POW(int zExponent) {
		return mPOW2[zExponent];
	}
	
	public int getSuperBlockLevel(MiniHash zData) {
		for(int i=MAX_LEVELS-1;i>=0;i--) {
			String hex = zData.toPureHexString();
			
			BigInteger val = new BigInteger(hex, 16);
		
			if(val.compareTo(mSuperLevels[i]) <= 0) {
				return i;
			}
		}
		
		return -1;
	}
	
	public BigInteger getSuperValue(int zLevel) {
		return mSuperLevels[zLevel];
	}
	
}
