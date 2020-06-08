package org.minima.utils;

import java.util.Random;

public class tester {

	public static void main(String[] zArgs) {
		System.out.println("Starting test..");
		
		Random rand = new Random();
		
		int counter = 0;
		while(true) {
			try {
				Thread.sleep(1000);
			
					
				if(rand.nextDouble()<0.5) {
					throw new OutOfMemoryError("ERROR!");
				}
			
			} catch (OutOfMemoryError e) {
				System.out.println("memory "+e);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				System.out.println("interrupt "+e);
			}	
			
			System.out.println("Counter "+counter);
			counter++;
		}
		
	}
	
}
