package org.minima.utils.tests.numbers;

import java.math.BigInteger;

import org.minima.utils.MinimaLogger;

public class SBlocks {

	public static void main(String[] zArgs) {
		
		int max = 256;
		
		BigInteger[] sbl = new BigInteger[max];
		
		BigInteger two = new BigInteger("2");
		for(int i=0;i<max;i++) {
			sbl[i] =  two.pow(i);
		
		
			MinimaLogger.log(i+")    0x"+ sbl[i].toString(16));
			
			
			
			
		}
		
				
		
	}
}
