package org.minima.database.coindb.java;

import java.util.ArrayList;

import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class JavaCoinDB implements CoinDB{

	public static long COINDB_LIMIT = 1000;
	
	ArrayList<CoinDBRow> mRows;
	
	public JavaCoinDB() {
		clearDB();
	}
	
	@Override
	public void clearDB() {
//		SimpleLogger.log("CLEAR COIN DB!");
		mRows = new ArrayList<>();
	}
	
//	@Override
//	public void clearOldCoins(long zCurrentBlock) {
//		ArrayList<CoinDBRow> newrows = new ArrayList<>();
//		for(CoinDBRow row : mRows) {
//			boolean old = row.isInBlock() && row.getInBlockNumber()<(zCurrentBlock-COINDB_LIMIT);
//			
//			if(!old) {
//				newrows.add(row);
//			}
//		}
//		mRows = newrows;
//	}

	@Override
	public ArrayList<CoinDBRow> getComplete() {
		return mRows;
	}

	@Override
	public ArrayList<CoinDBRow> checkForRelevantCoins(MiniData32 zAddress) {
		ArrayList<CoinDBRow> ret = new ArrayList<>();
		for(CoinDBRow row : mRows) {
			if(row.getCoin().getAddress().isExactlyEqual(zAddress)) {
				ret.add(row);
			}
		}
		return ret;
	}

	@Override
	public CoinDBRow getCoinRow(MiniData32 zCoinID) {
		for(CoinDBRow row : mRows) {
			if(row.getCoin().getCoinID().isNumericallyEqual(zCoinID)) {
				return row;
			}
		}
		return null;
	}

	@Override
	public CoinDBRow addCoinRow(Coin zCoin) {
		CoinDBRow row = getCoinRow(zCoin.getCoinID());
		if(row!=null) {
			return row;	
		}
		
		//Create a new Coin
		row = new JavaCoinDBRow(zCoin);
		mRows.add(row);
		
		return row;
	}

	@Override
	public void removeOldSpentCoins(MiniNumber zMinBlock) {
		ArrayList<CoinDBRow> newrows = new ArrayList<>();
		for(CoinDBRow row : mRows) {
			if(!row.isSpent()) {
				newrows.add(row);
			}else if(row.getInBlockNumber().isMoreEqual(zMinBlock)) {
				newrows.add(row);
			}
		}
		mRows = newrows;
	}

}
