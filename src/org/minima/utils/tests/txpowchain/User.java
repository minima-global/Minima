package org.minima.utils.tests.txpowchain;

import java.util.Random;

import org.minima.utils.MinimaLogger;

public class User {

	/**
	 * How much work on average should a user do in secs
	 */
	public static final double TXPOW_TIME = 10.0;
	
	/**
	 * How many transactions per second.. here 1 per blocktime
	 */
	public static final double TXN_PER_SEC = 1.0 / Simulator.BLOCKTIME; 
	
	
	int UID=0;
	
	Random mHash;
	
	int mTotalTXNFound = 0;
	
	double mCurrentTime = 0;
	int mCurrentBlock = 0;
	
	boolean mSendingTXN;
	int mBlockNumAtStart=0;
	
	double mTxnReliabilityCounter=0;
	
	public User(int zUID) {
		mHash = new Random();
		
		UID = zUID;
		
		mSendingTXN = false;
	}
	
	/**
	 * Update this user by 1 second
	 */
	public int Update(double zTimeInSecs, int zCurrentBlock) {
		//Store for logging
		mCurrentTime = zTimeInSecs;
		
		//A nice random number
		double newHash = mHash.nextDouble();
				
		//Put in a possible delay to receiving up to date info..
		if(mTxnReliabilityCounter<=0) {
			if(newHash<Simulator.TXN_RELIABILITY) {
				mCurrentBlock = zCurrentBlock;
			}else {
				mTxnReliabilityCounter = Simulator.RELIABILITY_DOWNTIME;
			}
		}else {
			mTxnReliabilityCounter--;
		}
				
		//Are we currently trying to send one ?
		if(mSendingTXN) {
			
			double sendTxnDiff = 1 / TXPOW_TIME;
			
			double txnperblock = TXN_PER_SEC * Simulator.BLOCKTIME * Simulator.NUM_USERS;
			
			double blockDiff   = sendTxnDiff / txnperblock;
			
			int ret = -1;
			
			if(newHash <= blockDiff ) {
				//Found a block!
				ret = mBlockNumAtStart+1;
				
			}else if(newHash <= sendTxnDiff) {
				//Just a txn
				ret=0;
			}
			
			if(ret>=0) {
				mTotalTXNFound++;
				mSendingTXN = false;
			}
			
			return ret;
			
		}else {
			//See if we need to start - USers send 
//			double starttxndiff = TXN_PER_SEC;
			
			//since you spend time hashing.. add a little..so 20% for 10 secs every 50
			double extra = (1 + (TXPOW_TIME * TXN_PER_SEC));
			double starttxndiff = TXN_PER_SEC * extra;
			
			if(newHash <= starttxndiff) {
				//Game On!
				mSendingTXN = true;
				mBlockNumAtStart = mCurrentBlock;
			}
		}
		
		return -1;
	}
	
	
	public void log(String zLog) {
		MinimaLogger.log("User["+UID+"] @ "+mCurrentTime+"s : "+zLog);
	}
}
