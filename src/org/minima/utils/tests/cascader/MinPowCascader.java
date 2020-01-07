package org.minima.utils.tests.cascader;

import java.util.Random;

import org.minima.utils.MinimaLogger;

public class MinPowCascader {

	public static void main(String[] zArgs) {
		
		Random rand = new Random();
		
		int pows=0;
		int longestwait = 0;
		double avgwait  = 0;
		for(int runs=0;runs<50;runs++) {
			
			boolean found = false;
			for(int i=1;i<1000;i++) {
				
				//Get a random hash
				double dd = rand.nextDouble();
				
				//Is it enough to replace all that went before..
				double currenttot = 1 / (double)i;
	
	//			SimpleLogger.log(i+") "+dd+"  \tmin:"+currenttot);
				
				if(i>=2) {
					if(dd < currenttot) {
						//It's enough..
						MinimaLogger.log(runs+") Higher level @ "+i+") \n" + dd+" / "+currenttot );
						found=true;
						pows++;
						if(i > longestwait) {
							longestwait = i;
						}
						
						avgwait += i;
						break;
					}
				}
			}
			
			if(!found) {
				MinimaLogger.log(runs+") POW NOT FOUND..!");
			}
		}
		
		//Totals..
		MinimaLogger.log("\nTotal Higher POWS found "+pows+" / 50" );
		MinimaLogger.log("Longest wait : "+longestwait);	
		MinimaLogger.log("AVG wait : "+(int)(avgwait / pows));	
		
		
	}
}
