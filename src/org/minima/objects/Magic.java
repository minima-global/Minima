package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Magic implements Streamable {

	/**
	 * The Current MAGIC numbers.. based on a weighted average of the chain..
	 */
	public MiniNumber mCurrentMaxTxPoWSize          = new MiniNumber(20000);
	public MiniNumber mCurrentMaxTxnPerBlock        = new MiniNumber(32);
	public MiniNumber mCurrentMaxKISSVMInstructions = new MiniNumber(128);
	
	/**
	 * The Desired MAGIC numbers.. user sets this..
	 */
	public MiniNumber mDesiredMaxTxPoWSize          = new MiniNumber(20000);
	public MiniNumber mDesiredMaxTxnPerBlock        = new MiniNumber(32);
	public MiniNumber mDesiredMaxKISSVMInstructions = new MiniNumber(128);
	
	
	public Magic() {}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("maxtxpow", mDesiredMaxTxPoWSize.getAsInt());
		magic.put("maxtxn", mDesiredMaxTxnPerBlock.getAsInt());
		magic.put("maxkissvm", mDesiredMaxKISSVMInstructions.getAsInt());
		
		return magic;
	}
	
	
	public void setToZero() {
		mCurrentMaxKISSVMInstructions 	= MiniNumber.ZERO;
		mCurrentMaxTxPoWSize 			= MiniNumber.ZERO;
		mCurrentMaxTxnPerBlock			= MiniNumber.ZERO;
	}
	
	/**
	 * Calcualte the current desired MAX values by taking a 
	 * weighted average of the last 128 blocks at EACH super block level for the top
	 * 16 levels.. this is deterministic and THE SAME for every user!
	 */
	public void calculateCurrentMax(BlockTreeNode mTip) {
		//How many levels to average
		int LEVELS 		= 16;
		int MAX_ADDED 	= 128;
		
		//An array of totals..
		int[] numadded 		= new int[LEVELS];
		Magic[] totals 		= new Magic[LEVELS];
		
		//Set to ZERO..
		for(int l=0;l<LEVELS;l++) {
			numadded[l] = 0;
			
			totals[l] 	= new Magic();
			totals[l].setToZero();
		}
		
		//Cycle through the chain.. adding to each level accumulator..
		BlockTreeNode current 		= mTip;
		while(current != null) {
			//What Level is this..
			int slevel = current.getSuperBlockLevel();
			
			//Get the Magic
			Magic mag = current.getTxPow().getMagic();
			
			//Add the desired to each level below this..
			for(int i=0;i<=slevel;i++) {
				//The multiplier..
				MiniNumber multiplier = MiniNumber.TWO.pow(i);
				
				if(numadded[i]<MAX_ADDED) {
					//Increment 
					numadded[i]++;
					
					//Add to the totals..
					totals[i].mCurrentMaxKISSVMInstructions = 
							totals[i].mCurrentMaxKISSVMInstructions.add(mag.mDesiredMaxKISSVMInstructions);
					totals[i].mCurrentMaxTxPoWSize = 
							totals[i].mCurrentMaxTxPoWSize.add(mag.mDesiredMaxTxPoWSize);
					totals[i].mCurrentMaxTxnPerBlock = 
							totals[i].mCurrentMaxTxnPerBlock.add(mag.mDesiredMaxTxnPerBlock);
					
				}
				
				
			}
			
			
			
			//Get ther parent..
			current = current.getParent();
		}
		
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
