package org.minima.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniHash;

public class SuperBlockLevels {

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
