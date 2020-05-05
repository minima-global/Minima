package org.minima.database.coindb.java;

import org.minima.database.coindb.CoinDBRow;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class JavaCoinDBRow implements CoinDBRow{

	Coin mCoin;
	
	boolean mIsSpent;
	
	boolean mIsInBlock;
	MiniNumber mInBlockNumber = new MiniNumber(0);
	
	MiniInteger mEntryNumber  = new MiniInteger(0);
	
	boolean mRelevant;
	
	boolean mKeeper;
	
	public JavaCoinDBRow(Coin zCoin) {
		mCoin 			= zCoin;
		mIsSpent 		= false;
		mIsInBlock		= false;
		mRelevant       = false;
		mKeeper         = false;
	}

	@Override
	public String toString() {
		return toJSON().toString();
	}

	@Override
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("mmrentry",getMMREntry().toString());
		ret.put("spent",mIsSpent);
		ret.put("relevant",mRelevant);
		ret.put("keeper",mKeeper);
		ret.put("isinblock",mIsInBlock);
		ret.put("inblock",mInBlockNumber.toString());
		ret.put("coin",mCoin.toJSON());
		
		return ret;
	}
	
	@Override
	public Coin getCoin() {
		return mCoin;
	}

	@Override
	public void setIsSpent(boolean zSpent) {
		mIsSpent = zSpent;
	}

	@Override
	public boolean isSpent() {
		return mIsSpent;
	}

	@Override
	public void setInBlockNumber(MiniNumber zInBlockNumber) {
		mInBlockNumber = zInBlockNumber;
	}

	@Override
	public MiniNumber getInBlockNumber() {
		return mInBlockNumber;
	}

	@Override
	public void setIsInBlock(boolean zIn) {
		mIsInBlock = zIn;
	}

	@Override
	public boolean isInBlock() {
		return mIsInBlock;
	}

	@Override
	public void setMMREntry(MiniInteger zEntry) {
		mEntryNumber = zEntry;
	}

	@Override
	public MiniInteger getMMREntry() {
		return mEntryNumber;
	}

	@Override
	public void setRelevant(boolean zRelevant) {
		mRelevant = zRelevant;
	}

	@Override
	public boolean isRelevant() {
		return mRelevant;
	}
	
	@Override
	public void setKeeper(boolean zKeeper) {
		mKeeper = zKeeper;
	}

	@Override
	public boolean isKeeper() {
		return mKeeper;
	}

}
