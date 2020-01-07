package org.minima.database.coindb;

import java.util.ArrayList;

import org.minima.utils.MinimaLogger;

public class CoinDBPrinter {

	public static void Print(CoinDB zDB){
	
		
		MinimaLogger.log("------");
		MinimaLogger.log("COINDB");
		MinimaLogger.log("------");
		
		
		ArrayList<CoinDBRow> rows = zDB.getComplete();
		MinimaLogger.log("Total COINDB Size : "+rows.size());
		
		int counter=0;
		for(CoinDBRow row : rows) {
//			if(!row.isSpent()) {
				MinimaLogger.log(counter+") "+row);
				counter++;
//			}
		}
		if(counter==0) {MinimaLogger.log("No coins..");}
	
	}
}
