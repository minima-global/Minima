package org.minima.utils;

import java.math.BigInteger;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniData;

public class SuperBlockLevels {

	public static MiniData GENESIS_HASH = new MiniData();
	
	private static BigInteger TWO = new BigInteger("2");
	
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
	
	public static void main(String[] zArgs) {
	
		MiniData diff = new MiniData("0xFF");
		MiniData act  = new MiniData("0x7F");
		
		int sup = getSuperLevel(diff, act);
		
		System.out.println(diff+" "+act+" "+sup);
		
		
		
		
	}
	
}
