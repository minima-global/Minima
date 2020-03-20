package org.minima.objects;

import java.math.BigInteger;

import org.minima.objects.base.MiniHash;

public class Difficulty {
	
	/**
	 * MAX 32 Byte Value 
	 */
	public static final BigInteger MAX_VAL = 
			new BigInteger(	"FFFFFFFFFFFFFFFFFFFF"+
							"FFFFFFFFFFFFFFFFFFFF"+
							"FFFFFFFFFFFFFFFFFFFF"+
							"FFFF",16);
	
	/**
	 * The Value 2.. used a lot..
	 */
	public static final BigInteger DIFF_TWO = new BigInteger("2");
	
	/**
	 * The initial value from 0-255;
	 */
	int mValue;
	
	/**
	 * The actual value that  hash must be lower than to be valid
	 */
	BigInteger mDifficulty;
	
	/**
	 * The higher the value the higher the difficulty
	 * 
	 * When comparing values with difficulty, 
	 * they must be LOWER than the difficulty to pass.
	 * 
	 * @param zDifficulty
	 */
	public Difficulty(int zDifficulty) {
		mValue = zDifficulty;
		
		//Create a BigInteger Value..
//		mDifficulty = MAX_VAL.subtract(DIFF_TWO.pow(zDifficulty)) ;
		mDifficulty = MAX_VAL.divide(DIFF_TWO.pow(zDifficulty)) ;
	}
	
	public boolean isOK(MiniHash zValue) {
		return zValue.getDataVaue().compareTo(mDifficulty) <= 0;
	}

	@Override
	public String toString() {
		return "0x"+mDifficulty.toString(16);
	}
}
