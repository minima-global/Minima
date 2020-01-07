package org.minima.database.mmr;

import java.util.ArrayList;
import java.util.Collections;

public class MMRPrint {

	public static void Print(MMRSet zSet) {
		Print(zSet, true);
	}
	
	public static void Print(MMRSet zSet, boolean zComplete) {
		if(!zComplete) {
			PrintSet(zSet);
			return;
		}
		
		//Cycle through the Sets..
		MMRSet current = zSet;
		while(current != null) {
			PrintSet(current);
			
			//See if there is a parent..
			current = current.getParent();
		}
	}
	
	private static void PrintSet(MMRSet zSet) {
		MMRSet current = zSet;
		System.out.println("BLK:"+current.getBlockTime() + " KEEPER:"+current.getKeepers());
		System.out.println("Root  : "+current.getMMRRoot());
		System.out.println("Peaks : "+current.getMMRPeaks());
		
		//Max Row
		int max = current.getMaxRow();
		
		for(int i=max;i>=0;i--) {
			ArrayList<MMREntry> entries = current.getRow(i);
			
			//Sort.
			Collections.sort(entries);
			
			for(MMREntry entry : entries) {
				System.out.println(entry);
			}
		}	
		
		System.out.println("");
	}
}
