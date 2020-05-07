package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxHeader implements Streamable {

	/**
	 * The NONCE - the user definable data you cycle through to change the final hash of this TxPow
	 */
	public MiniInteger mNonce;
	
	/**
	 * Time Secs  
	 */
	public MiniNumber 	mTimeSecs;
	
	/**
	 * The Block Number
	 */
	public MiniNumber  mBlockNumber;
	
	/**
	 * The BASE Block Difficulty
	 */
	public MiniData mBlockDifficulty;
	
	/**
	 * A list of all the parent blocks at all the Super Block Levels..
	 */
	public MiniData[] mSuperParents;
	
	/**
	 * A Chain ID. Useful when running side-chains, as only this TokenID will be valid to POS mine it. 
	 * 0x00 is the main chain
	 */
	public MiniData mChainID = new MiniData("0x00");
	
	/**
	 * Every Side chain has a parent chain
	 */
	public MiniData mParentChainID = new MiniData("0x00");
	
	/**
	 * The HASH of the TxBody
	 */
	public MiniData mTxBody;
	
	/**
	 * In the long run ONLY this header is kept and the body is discarded..
	 */
	public TxHeader() {
		//How many super block levels..
		mSuperParents = new MiniData[GlobalParams.MINIMA_CASCADE_LEVELS];
		
		//Super Block Levels..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			mSuperParents[i] = new MiniData();
		}
	}

	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("block", mBlockNumber.toString());
		
		txpow.put("isblock", _mIsBlockPOW);
		txpow.put("txpowid", _mTxPOWID.toString());
		txpow.put("parent", mParent.toString());
		
		//The Super parents are efficiently encoded in RLE
		JSONArray supers = new JSONArray();
		MiniData old = null;
		int counter=0;
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			MiniData curr = mSuperParents[i];
			
			if(old == null) {
				old = curr;
				counter++;
			}else {
				if(old.isEqual(curr)) {
					counter++;
					//Is this the last one..
					if(i==GlobalParams.MINIMA_CASCADE_LEVELS-1) {
						//Write it anyway..
						JSONObject sp = new JSONObject();
						sp.put("difficulty", i);
						sp.put("count", counter);
						sp.put("parent", curr.to0xString());
						supers.add(sp);						
					}
					
				}else {
					//Write the old one..
					JSONObject sp = new JSONObject();
					sp.put("difficulty", i);
					sp.put("count", counter);
					sp.put("parent", old.to0xString());
					supers.add(sp);
					
					//Reset
					old=curr;
					counter=1;
				}
			}
		}
		txpow.put("superparents", supers);
		
		txpow.put("blkdiff", mBlockDifficulty.to0xString());
		txpow.put("superblock", _mSuperBlock);
		
		txpow.put("chainid", mChainID.toString());
		txpow.put("parentchainid", mParentChainID.toString());
		txpow.put("nonce", mNonce.toString());
		
		txpow.put("timesecs", mTimeSecs.toString());
		txpow.put("date", new Date(mTimeSecs.getAsLong()*1000).toString());
		
		return txpow;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
	}
}
