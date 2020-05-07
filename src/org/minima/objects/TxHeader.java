package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniByte;
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
	public MiniInteger mNonce = new MiniInteger(0);
	
	/**
	 * Time Secs  
	 */
	public MiniInteger 	mTimeMilli;
	
	/**
	 * The Block Number - needs to be a MiniNumber as is used in Scripts..
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
	public MiniData mTxBodyHash;
	
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
	
	public MiniData getBodyHash() {
		return mTxBodyHash;
	}

	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("block", mBlockNumber.toString());
		txpow.put("blkdiff", mBlockDifficulty.to0xString());
		
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
		
		txpow.put("chainid", mChainID.toString());
		txpow.put("parentchainid", mParentChainID.toString());
		
		txpow.put("nonce", mNonce.toString());
		txpow.put("timemilli", mTimeMilli.toString());
		txpow.put("date", new Date(new Long(mTimeMilli+"")).toString());
		
		return txpow;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mNonce.writeDataStream(zOut);
		mTimeMilli.writeDataStream(zOut);
		mBlockNumber.writeDataStream(zOut);
		mBlockDifficulty.writeDataStream(zOut);
		mChainID.writeDataStream(zOut);
		mParentChainID.writeDataStream(zOut);
		
		//The Super parents are efficiently encoded in RLE
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
						MiniByte count = new MiniByte(counter);
						count.writeDataStream(zOut);
						curr.writeDataStream(zOut);						
					}
					
				}else {
					//Write the old one..
					MiniByte count = new MiniByte(counter);
					count.writeDataStream(zOut);
					old.writeDataStream(zOut);
					
					//Reset
					old=curr;
					counter=1;
				}
			}
		}
		
		//Write the Boddy Hash
		mTxBodyHash.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mNonce           = MiniInteger.ReadFromStream(zIn);
		mTimeMilli       = MiniInteger.ReadFromStream(zIn);
		mBlockNumber     = MiniNumber.ReadFromStream(zIn);
		mBlockDifficulty = MiniData.ReadFromStream(zIn);
		mChainID         = MiniData.ReadFromStream(zIn);
		mParentChainID   = MiniData.ReadFromStream(zIn);
		
		//And the super parents - RLE
		int tot   = 0;
		while(tot<GlobalParams.MINIMA_CASCADE_LEVELS) {
			MiniByte len = MiniByte.ReadFromStream(zIn);
			MiniData sup = MiniData.ReadFromStream(zIn);
			int count = len.getValue();
			for(int i=0;i<count;i++) {
				mSuperParents[tot++] = sup;
			}
		}
		
		//The TxBody Hash
		mTxBodyHash          = MiniData.ReadFromStream(zIn);
	}
}
