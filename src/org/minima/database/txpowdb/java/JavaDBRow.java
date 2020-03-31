package org.minima.database.txpowdb.java;

import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;

public class JavaDBRow implements TxPOWDBRow {

	private TxPOW mTxPOW;

	private boolean mIsOnChainBlock;
	
	private boolean mIsInBlock;
	
	private MiniNumber mInBlocknumber;
	
	private int mBlockState;
	
	private long mDeleteTime;
	
	public JavaDBRow(TxPOW zTxPOW) {
		mTxPOW 				= zTxPOW;
		mIsInBlock 			= false;
		mIsOnChainBlock     = false;
		mBlockState         = TXPOWDBROW_STATE_BASIC;
		mDeleteTime          = 0;
	}

	@Override
	public TxPOW getTxPOW() {
		return mTxPOW;
	}

	@Override
	public void setIsInBlock(boolean zIsInBlock) {
		mIsInBlock = zIsInBlock;
	}
	
	@Override
	public boolean isInBlock() {
		return mIsInBlock;
	}

	@Override
	public MiniNumber getInBlockNumber() {
		return mInBlocknumber;
	}

	@Override
	public void setInBlockNumber(MiniNumber zBlockNumber) {
		mInBlocknumber = zBlockNumber;
	}
	
	@Override
	public String toString() {
		return getStatusAsString()+" ONCHAINBLK:"+isOnChainBlock()+" IS_IN_BLOCK ("+mIsInBlock+") :"+mInBlocknumber+" "+mTxPOW;
	}

	public String getStatusAsString() {
		switch (mBlockState) {
		case TXPOWDBROW_STATE_BASIC:
			return "BASIC";
		case TXPOWDBROW_STATE_FULL:
			return "FULL";
//		case TXPOWDBROW_STATE_VALID:
//			return "VALID";
//		case TXPOWDBROW_STATE_INVALID:
//			return "INVALID";

		default:
			return "ERROR";
		}
	}
	
	@Override
	public boolean isOnChainBlock(){
		return mIsOnChainBlock;
	}
	
	@Override
	public void setOnChainBlock(boolean zOnChain) {
		mIsOnChainBlock = zOnChain;
	}

	@Override
	public int getBlockState() {
		return mBlockState;
	}

	@Override
	public void setBlockState(int zState) {
		mBlockState = zState;
	}

	@Override
	public void deleteRow() {
		if(mDeleteTime == 0) {
			mDeleteTime = System.currentTimeMillis();
//			System.out.println("Row deleted "+mTxPOW.getBlockNumber()+" "+mDeleteTime);
		}
	}

	@Override
	public long getDeleteTime() {
		return mDeleteTime;
	}
}
