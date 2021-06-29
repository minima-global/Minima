package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Magic implements Streamable {

	/**
	 * Default starting values..
	 */
	public static final MiniNumber MIN_TXPOW_DEFAULT 	= new MiniNumber(20000);
	public static final MiniNumber MIN_TXPOW_TXNS 	 	= new MiniNumber(100);
	public static final MiniNumber MIN_KISSVM_INST		= new MiniNumber(128);
	
	/**
	 * The Current MAGIC numbers.. based on a weighted average of the chain..
	 */
	public MiniNumber mCurrentMaxTxPoWSize          = new MiniNumber(20000);
	public MiniNumber mCurrentMaxTxnPerBlock        = new MiniNumber(32);
	public MiniNumber mCurrentMaxKISSVMInstructions = new MiniNumber(128);
	
	/**
	 * The Desired MAGIC numbers.. user sets this..
	 */
	public MiniNumber mDesiredMaxTxPoWSize          = MIN_TXPOW_DEFAULT;
	public MiniNumber mDesiredMaxTxnPerBlock        = MIN_TXPOW_TXNS;
	public MiniNumber mDesiredMaxKISSVMInstructions = MIN_KISSVM_INST;
	
	public Magic() {}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("desiredmaxtxpow", mDesiredMaxTxPoWSize.getAsInt());
		magic.put("desiredmaxtxn", mDesiredMaxTxnPerBlock.getAsInt());
		magic.put("desiredmaxkissvm", mDesiredMaxKISSVMInstructions.getAsInt());
		
		magic.put("maxtxpow", mCurrentMaxTxPoWSize.getAsInt());
		magic.put("maxtxn", mCurrentMaxTxnPerBlock.getAsInt());
		magic.put("maxkissvm", mCurrentMaxKISSVMInstructions.getAsInt());
		
		return magic;
	}
	
	
	public void setToZero() {
		mCurrentMaxKISSVMInstructions 	= MiniNumber.ZERO;
		mCurrentMaxTxPoWSize 			= MiniNumber.ZERO;
		mCurrentMaxTxnPerBlock			= MiniNumber.ZERO;
	}
	
	/**
	 * Calculate the current desired MAX values by taking a 
	 * weighted average of the last 128 blocks at EACH super block level
	 */
	public void calculateCurrentMax(BlockTreeNode mTip) {
		//How many levels to average
		int MAX_ADDED 	= 3;
		int MAX_LEVEL 	= 3;
		
		//An array of totals..
		MiniNumber tnum 	= MiniNumber.ZERO;
		int[] numadded 		= new int[MAX_LEVEL];
		
		//Running totals
		Magic ctotal 		= new Magic();
		ctotal.setToZero();
		
		//Set to ZERO..
		for(int l=0;l<MAX_LEVEL;l++) {
			numadded[l] = 0;
		}
		
		//Cycle through the chain.. adding to each level accumulator..
		BlockTreeNode current 		= mTip;
		while(current != null) {
			//What Level is this..
			int slevel = current.getSuperBlockLevel();
			
			//Get the Magic
			Magic mag = current.getTxPow().getMagic();
			
			//Check upto super block level 12 which is 120 days worth
			//Whcih are 30, 60, and 120 days..so 3 months..
			for(int num=0;num<MAX_LEVEL;num++) {
				if(slevel >= num) {
					if(numadded[num]<MAX_ADDED) {
						numadded[num]++;
						MiniNumber multiplier = MiniNumber.TWO.pow(num);
						ctotal.mCurrentMaxTxPoWSize = 
								ctotal.mCurrentMaxTxPoWSize.add(mag.mDesiredMaxTxPoWSize.mult(multiplier));
						ctotal.mCurrentMaxTxnPerBlock = 
								ctotal.mCurrentMaxTxnPerBlock.add(mag.mDesiredMaxTxnPerBlock.mult(multiplier));
						ctotal.mCurrentMaxKISSVMInstructions = 
								ctotal.mCurrentMaxKISSVMInstructions.add(mag.mDesiredMaxKISSVMInstructions.mult(multiplier));
						
						tnum=tnum.add(multiplier);
						//MinimaLogger.log(num+" CT:"+ctotal.mCurrentMaxTxPoWSize+" "+mag.mDesiredMaxTxPoWSize+" "+tnum.toString());
					}
				}
			}
			
			//Get ther parent..
			current = current.getParent();
		}
		
		//Now do dividion..
		if(tnum.isEqual(MiniNumber.ZERO)) {
			ctotal.mCurrentMaxTxPoWSize 			= MIN_TXPOW_DEFAULT;
			ctotal.mCurrentMaxTxnPerBlock 			= MIN_TXPOW_TXNS;
			ctotal.mCurrentMaxKISSVMInstructions 	= MIN_KISSVM_INST;
		}else {
			ctotal.mCurrentMaxTxPoWSize 			= ctotal.mCurrentMaxTxPoWSize.div(tnum).floor();
			ctotal.mCurrentMaxTxnPerBlock 			= ctotal.mCurrentMaxTxnPerBlock.div(tnum).floor();
			ctotal.mCurrentMaxKISSVMInstructions	= ctotal.mCurrentMaxKISSVMInstructions.div(tnum).floor();
		}
		
		//And set it
		mCurrentMaxTxPoWSize 			= ctotal.mCurrentMaxTxPoWSize;
		mCurrentMaxTxnPerBlock 			= ctotal.mCurrentMaxTxnPerBlock;
		mCurrentMaxKISSVMInstructions 	= ctotal.mCurrentMaxKISSVMInstructions;
		
		MinimaLogger.log("MAGIC "+tnum+" "+toJSON().toString());
	}
	
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCurrentMaxTxPoWSize.writeDataStream(zOut);
		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
		mCurrentMaxKISSVMInstructions.writeDataStream(zOut);
		
		mDesiredMaxTxPoWSize.writeDataStream(zOut);
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		mDesiredMaxKISSVMInstructions.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCurrentMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mCurrentMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mCurrentMaxKISSVMInstructions = MiniNumber.ReadFromStream(zIn);
		
		mDesiredMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxKISSVMInstructions = MiniNumber.ReadFromStream(zIn);
	}
	
	public static Magic ReadFromStream(DataInputStream zIn) throws IOException {
		Magic mag = new Magic();
		mag.readDataStream(zIn);
		return mag;
	}
}
