package org.minima.database.coindb.java;

import org.minima.database.coindb.CoinDBRow;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;

public class JavaCoinDBRow implements CoinDBRow{

	Coin mCoin;
	
	boolean mIsSpent;
	
	boolean mIsInBlock;
	MiniNumber mInBlockNumber = new MiniNumber(0);
	
	MiniInteger mEntryNumber  = new MiniInteger(0);
	
	public JavaCoinDBRow(Coin zCoin) {
		mCoin 			= zCoin;
		mIsSpent 		= false;
		mIsInBlock		= false;
	}

	@Override
	public String toString() {
		return "MMR:"+getMMREntry()+" spent:"+mIsSpent+" inblock:"+mIsInBlock+" block:"+mInBlockNumber+" "+mCoin;
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
}
