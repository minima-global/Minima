package org.minima.database.coindb.java;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class FastCoinDB implements CoinDB {

	Hashtable<String, JavaCoinDBRow> mCoins;
	
	public FastCoinDB() {
		mCoins = new Hashtable<>();
	}
	
	@Override
	public void clearDB() {
		mCoins.clear();
	}

	@Override
	public ArrayList<CoinDBRow> getComplete() {
		ArrayList<CoinDBRow> ret = new ArrayList<>();
		Enumeration<JavaCoinDBRow> allrows = mCoins.elements();
		while(allrows.hasMoreElements()) {
			JavaCoinDBRow row = allrows.nextElement();	
			ret.add(row);
		}
		return ret;
	}

	@Override
	public ArrayList<CoinDBRow> getCompleteRelevant() {
		ArrayList<CoinDBRow> ret = new ArrayList<>();
		Enumeration<JavaCoinDBRow> allrows = mCoins.elements();
		while(allrows.hasMoreElements()) {
			JavaCoinDBRow row = allrows.nextElement();	
			if(row.isRelevant() || row.isKeeper()) {
				ret.add(row);	
			}
		}
		return ret;
	}

	@Override
	public CoinDBRow getCoinRow(MiniData zCoinID) {
		return mCoins.get(zCoinID.to0xString());
	}

	@Override
	public CoinDBRow addCoinRow(Coin zCoin) {
		String search = zCoin.getCoinID().to0xString();
		
		//Is it already in there
		JavaCoinDBRow row = mCoins.get(search);
		if(row != null) {
			return row;
		}
		
		//Create it
		row = new JavaCoinDBRow(zCoin);
		mCoins.put(search, row);
		
		return row;
	}

	@Override
	public boolean removeCoin(MiniData zCoinID) {
		String search = zCoinID.to0xString();
		if(mCoins.contains(search)) {
			mCoins.remove(search);
			return true;
		}
		return false;
	}

	@Override
	public void removeOldSpentCoins(MiniNumber zMinBlock) {
		Hashtable<String, JavaCoinDBRow> newCoins = new Hashtable<>();
		Enumeration<JavaCoinDBRow> allrows = mCoins.elements();
		while(allrows.hasMoreElements()) {
			JavaCoinDBRow row = allrows.nextElement();	
			if(!row.isSpent() && ( row.isRelevant() || row.isKeeper() )) {
				newCoins.put(row.getCoin().getCoinID().to0xString(), row);
			
			}else if(row.getInBlockNumber().isMoreEqual(zMinBlock)) {
				newCoins.put(row.getCoin().getCoinID().to0xString(), row);
			}
		}	
		
		//Switch them..
		mCoins = newCoins;
	}

	@Override
	public void resetCoinsFomOnwards(MiniNumber zBlock) {
		Hashtable<String, JavaCoinDBRow> newCoins = new Hashtable<>();
		Enumeration<JavaCoinDBRow> allrows = mCoins.elements();
		while(allrows.hasMoreElements()) {
			JavaCoinDBRow row = allrows.nextElement();	
			if(row.getInBlockNumber().isLess(zBlock)) {
				newCoins.put(row.getCoin().getCoinID().to0xString(), row);	
			}
		}	
		
		//Switch them..
		mCoins = newCoins;
	}

}
