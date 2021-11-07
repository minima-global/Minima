package org.minima.database.txpowdb.ram;

import org.minima.objects.TxPoW;

public class RamData {

	TxPoW mTxPoW;
	
	long mLastAccess;
	
	boolean mIsOnMainChain = false;
	
	boolean mIsInCascade = false;
	
	public RamData(TxPoW zTxPoW) {
		mTxPoW = zTxPoW;
		mLastAccess = System.currentTimeMillis();
	}
	
	public TxPoW getTxPoW() {
		return mTxPoW;
	}
	
	public void updateLastAccess() {
		mLastAccess = System.currentTimeMillis();
	}
	
	public long getLastAccess() {
		return mLastAccess;
	}
	
	public void setOnMainChain(boolean zOnChain) {
		mIsOnMainChain = zOnChain;
	}
	
	public boolean isOnMainChain() {
		return mIsOnMainChain;
	}
	
	public void setInCascade(boolean zCascader) {
		mIsInCascade = zCascader;
	}
	
	public boolean isInCascade() {
		return mIsInCascade;
	}
}
