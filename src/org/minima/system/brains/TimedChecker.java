package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.TxPoW;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

public class TimedChecker {

	public static final long MAX_CHECKTIME = 120000;
	
	boolean mFinishedRunning;
	boolean mValidBlock;
	
	Thread mCheckerThread;
	
	public TimedChecker() {
		mFinishedRunning = false;
		mValidBlock		 = false;
	}
	
	public boolean checkTxPoWBlock(TxPoWTreeNode zParentNode, TxPoW zTxPoW, ArrayList<TxPoW> zTransactions) {
		
		long timenow  = System.currentTimeMillis();
		long timediff = 0;
		
		try {
			
			//Start a thread that does the checking..
			Runnable check = new Runnable() {
				
				@Override
				public void run() {
					
					try {
						
						//Is it a valid block
						mValidBlock = TxPoWChecker.checkTxPoWBlock(zParentNode, zTxPoW, zTransactions);
						
					} catch (Exception e) {
						MinimaLogger.log("Block failed to process : "+e.toString());
						mValidBlock = false;
					}
					
					//We have finished
					mFinishedRunning = true;
				}
			};
			
			//Create a separate thread
			mCheckerThread = new Thread(check);
			mCheckerThread.start();
			
			//Now check..
			while(timediff < MAX_CHECKTIME) {
				
				//Wait a second..
				Thread.sleep(50);
				
				//Has the previous thread finished..
				if(mFinishedRunning) {
					break;
				}
				
				//New time counter
				timediff = System.currentTimeMillis() - timenow;
			}
				
			//Stop the old thread..
			mCheckerThread.interrupt();
			
		} catch (Exception e) {
			MinimaLogger.log(e);
			mValidBlock = false;
		}
		
		//Are we logging this
		if(GeneralParams.BLOCK_LOGS) {
			timediff = System.currentTimeMillis() - timenow;
			MinimaLogger.log("[VALID:"+mValidBlock+"] Block checker time : "+timediff+"ms @ "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPoWID());
		}
		
		return mValidBlock;
	}
}
