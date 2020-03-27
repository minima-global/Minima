package org.minima.database.txpowdb;

import java.util.ArrayList;

import org.minima.utils.MinimaLogger;

public class TxPowDBPrinter {
	
	public static void PrintDB(TxPowDB zDB) {
		//Get the database..
		ArrayList<TxPOWDBRow> rows = zDB.getAllTxPOWDBRow();
		
		MinimaLogger.log("--------");
		MinimaLogger.log("TXPOW DB");
		MinimaLogger.log("--------");
		MinimaLogger.log("Total TXPOWDB Size : "+rows.size());
		
		int counter = 0;
		for(TxPOWDBRow row : rows) {
//			if(row.getTxPOW().isBlock()) {
				MinimaLogger.log(counter+" "+row);
				counter++;
//			}
		}
//		if(counter==0) {SimpleLogger.log("NO BLOCKS");}
		
//		counter = 0;
//		SimpleLogger.log("----");
//		SimpleLogger.log("TXNS");
//		SimpleLogger.log("----");
//		for(TxPOWDBRow row : rows) {
//			if(row.getTxPOW().isTransaction()) {
//				SimpleLogger.log(counter+" "+row);
//				counter++;
//			}
//		}
//		if(counter==0) {SimpleLogger.log("No txns..");}
		
	}
}
