package org.minima.utils.tests.cascader;

import java.util.Random;

import org.minima.utils.MinimaLogger;

public class POWChecker {

	public static final int NUMBER_BLOCKS = 10000000;
	public static final int NUMBER_LEVELS =  32;
	
	public static void main(String[] zArgs) {
		
		//Our hash generator
		Random rand = new Random();
		
		int[] levelCounters = new int[NUMBER_LEVELS];
		double[] totalHigherDiff = new double[NUMBER_LEVELS];
		
		int currentlevel=0;
		
		for (int i=0;i<NUMBER_BLOCKS;i++) {
			
			double diff = rand.nextDouble();

			//Check the Levels
			int maxlev = 0;
			for(int lev=0;lev<=currentlevel;lev++) {
				
				double POWrating = Math.pow(2.0, lev);
				
				double levdiff = 1.0 / POWrating;
				
				if(diff <  levdiff ) {
					maxlev = lev;
					
					//It's a hit..
					levelCounters[lev]++;
					totalHigherDiff[lev]+=POWrating;
					
					//Do we check a higher level
					if(lev>=currentlevel) {
						MinimaLogger.log("Level UP! @ Block "+i+" "+lev+" -> "+(currentlevel+1));
						currentlevel++;
						break;
					}
				}
			
			}
			
//			SimpleLogger.log("Hit : Level "+maxlev+" [ "+diff+" ]");
			
			
		}
		
		for(int i=0;i<NUMBER_LEVELS;i++) {
			double perc = 100.0 * (totalHigherDiff[i] / NUMBER_BLOCKS);
			MinimaLogger.log("Level : "+i+" ["+(int)perc+"%] "+levelCounters[i]+" Total:"+totalHigherDiff[i]+" Diff:"+(1.0 / Math.pow(2.0, i)));
		}
		
		
	}
	
	
}
