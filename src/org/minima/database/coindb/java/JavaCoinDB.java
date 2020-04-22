package org.minima.database.coindb.java;

import java.util.ArrayList;

import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class JavaCoinDB implements CoinDB{

	public static long COINDB_LIMIT = 1000;
	
	ArrayList<CoinDBRow> mRows;
	
	public JavaCoinDB() {
		clearDB();
	}
	
	@Override
	public void clearDB() {
		mRows = new ArrayList<>();
	}
	
	@Override
	public ArrayList<CoinDBRow> getComplete() {
		return mRows;
	}

	@Override
	public CoinDBRow getCoinRow(MiniData zCoinID) {
		for(CoinDBRow row : mRows) {
			if(row.getCoin().getCoinID().isEqual(zCoinID)) {
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

	@Override
	public boolean removeCoin(MiniData zCoinID) {
		boolean found=false;
		ArrayList<CoinDBRow> newrows = new ArrayList<>();
		for(CoinDBRow row : mRows) {
			if(!row.getCoin().getCoinID().isEqual(zCoinID)) {
				newrows.add(row);
			}else {
				found=true;
			}
		}
		mRows = newrows;
		
		return found;
	}

	
}
