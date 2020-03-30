/**
 * 
 */
package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.SuperBlockLevels;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

/**
 * 
 * @author Spartacus Rex
 *
 */
public class TxPOW implements Streamable {
	
	/**
	 * The NONCE - the user definable data you cycle through to change the final hash of this TxPow
	 */
	private MiniNumber 	mNonce	= new MiniNumber(0);
	
	/**
	 * Time Secs  
	 */
	private MiniNumber 	mTimeSecs= new MiniNumber();
	
	/**
	 * The Difficulty for this TXPOW to be valid.
	 * 
	 * This is calculated 2^n.. so level 255, the max, is 2^255
	 * 
	 * The amount of work (Proof-of-work) that has gone into it.
	 */
	
	private MiniHash 	mTxnDifficulty = new MiniHash();
	
	/**
	 * The Transaction the user is trying to send
	 */
	private Transaction	mTransaction = new Transaction();
	
	/**
	 * The Witness data for the Transaction
	 */
	private Witness		mWitness = new Witness();
	
	/**
	 * The BURN paying for the Transaction the user is trying to send - can be empty
	 */
	private Transaction	mBurnTransaction = new Transaction();
	
	/**
	 * The Witness data for the FeeTransaction - can be empty
	 */
	private Witness	   mBurnWitness = new Witness();
	
	/**
	 * The Block Number
	 */
	private MiniNumber  mBlockNumber = new MiniNumber();
	
	/**
	 * The previous TXPOW Block
	 */
	private MiniHash  mParent = new MiniHash();
	
	/**
	 * The BASE Block Difficulty
	 */
	private MiniHash mBlockDifficulty = new MiniHash();
	
	/**
	 * The list of the current TX-POWs the user 
	 * knows about that are not yet in the this chain.
	 */
	private ArrayList<MiniHash> mTxPowIDList;
	
	/**
	 * A list of all the parent blocks at all the Super Block Levels..
	 */
	public MiniHash[] mSuperParents = new MiniHash[GlobalParams.MINIMA_CASCADE_LEVELS];
	
	/**
	 * The MMR Root!
	 */
	public MiniHash mMMRRoot = new MiniHash();
	
	/**
	 * A Random Magic number so that everyone is working on a different TxPOW in the pulse 
	 * (since there is no coinbase..)
	 */
	public MiniHash mMagic = MiniHash.getRandomData();
	
	/**
	 * A Chain ID. Useful when running side-chains, as only this TokenID will be valid to POS mine it. 
	 * 0x00 is the main chain
	 */
	public MiniHash mChainID = new MiniHash("0x00");
	
	/**
	 * Every Side chain has a parent chain
	 */
	public MiniHash mParentChainID = new MiniHash("0x00");

	/**
	 * A Custom Hash. Can be anything the user wants..
	 */
	public MiniHash mCustom = new MiniHash("0x00");
	
	/**
	 * These are used internally ONLY
	 */
	private MiniHash _mTxPOWID = new MiniHash();
	private MiniHash _mTransID = new MiniHash();
	
	protected boolean _mIsBlockPOW  = false;
	protected boolean _mIsTxnPOW    = false;
	protected int     _mSuperBlock  = 0;
	
	/**
	 * Static date format
	 */
	private static SimpleDateFormat mSDF = new SimpleDateFormat("HH:mm:ss.SSS");
	
	/**
	 * Main Constructor
	 */
	public TxPOW() {
		mTxPowIDList = new ArrayList<>();
		
		//Super Block Levels..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			mSuperParents[i] = new MiniHash();
		}
	}
	
	public void setNonce(MiniNumber zNonce) {
		mNonce = zNonce;
	}
	
	public MiniNumber getNonce() {
		return mNonce;
	}
	
	public void setChainID(MiniHash zChainID) {
		mChainID = zChainID;
	}
	
	public void setParentChainID(MiniHash zChainID) {
		mParentChainID = zChainID;
	}
	
	public MiniHash getChainID() {
		return mChainID;
	}
	
	public MiniHash getParentChainID() {
		return mParentChainID;
	}
	
	public void setCustom(MiniHash zCustom) {
		mCustom = zCustom;
	}
	
	public MiniHash getCustom() {
		return mCustom;
	}
	
	public void setTxDifficulty(MiniHash zDifficulty) {
		mTxnDifficulty = zDifficulty;
	}
	
	public MiniHash getTxnDifficulty() {
		return mTxnDifficulty;
	}
	
	public void setTimeSecs(MiniNumber zSecs) {
		mTimeSecs = zSecs;
	}
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public Transaction getBurnTransaction() {
		return mBurnTransaction;
	}
	
	public void setTransaction(Transaction zTran) {
		mTransaction = zTran;
	}
	
	public void addBlockTxPOW(TxPOW zTxPOW) {
		mTxPowIDList.add(zTxPOW.getTxPowID());
	}
	
	public ArrayList<MiniHash> getBlockTxns(){
		return mTxPowIDList;
	}
	
	public MiniHash getBlockDifficulty() {
		return mBlockDifficulty;
	}
	
	public void setBlockDifficulty(MiniHash zBlockDifficulty) {
		mBlockDifficulty = zBlockDifficulty;
	}
	
	public void setParent(MiniHash zData) {
		mParent = zData;
	}
	
	public MiniHash getParentID() {
		return mParent;
	}
	
	public MiniHash getSuperParent(int zLevel) {
		return mSuperParents[zLevel];
	}
	
	public MiniNumber getTimeSecs() {
		return mTimeSecs;
	}
	
	public void setBlockNumber(MiniNumber zBlockNum) {
		mBlockNumber = zBlockNum;
	}
	
	public MiniNumber getBlockNumber() {
		return mBlockNumber;
	}
	
	public void setWitness(Witness zWitness) {
		mWitness = zWitness;
	}
	
	public Witness getWitness() {
		return mWitness;
	}
	
	public Witness getBurnWitness() {
		return mBurnWitness;
	}
	
	public MiniHash getMMRRoot() {
		return mMMRRoot;
	}
	
	public void setMMRRoot(MiniHash zRoot) {
		mMMRRoot = zRoot;
	}
	
	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("block", mBlockNumber.toString());
		txpow.put("isblock", _mIsBlockPOW);
		txpow.put("txpowid", _mTxPOWID.toString());
		txpow.put("parent", mParent.toString());
		
		//The Super parents are efficiently encoded in RLE
		JSONArray supers = new JSONArray();
		MiniHash old = null;
		int counter=0;
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			MiniHash curr = mSuperParents[i];
			
			if(old == null) {
				old = curr;
				counter++;
			}else {
				if(old.isExactlyEqual(curr)) {
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
		
//		//The Super parents
//		JSONArray supers = new JSONArray();
//		for(int i=0;i<16;i++) {
//			supers.add(mSuperParents[i].toShort0xString(16));
//		}
//		txpow.put("superparents", supers);
		
		txpow.put("blkdiff", mBlockDifficulty.to0xString());
		txpow.put("superblock", _mSuperBlock);
		txpow.put("txndiff", mTxnDifficulty.to0xString());
		txpow.put("txn", mTransaction.toJSON());
		txpow.put("witness", mWitness.toJSON());
		
		//The BURN transaction.. normally empty
		txpow.put("burntxn", mBurnTransaction.toJSON());
		txpow.put("burnwitness", mBurnWitness.toJSON());
		
		//Need to make it into a JSON array
		JSONArray txns = new JSONArray();
		for(MiniHash txn : mTxPowIDList) {
			txns.add(txn.to0xString());
		}
		txpow.put("txnlist", txns);
		
		txpow.put("magic", mMagic.toString());
		txpow.put("chainid", mChainID.toString());
		txpow.put("parentchainid", mParentChainID.toString());
		txpow.put("custom", mCustom.toString());
		txpow.put("nonce", mNonce.toString());
		txpow.put("mmr", mMMRRoot.toString());
		
		txpow.put("timesecs", mTimeSecs.toString());
		txpow.put("date", new Date(mTimeSecs.getAsLong()*1000).toString());
		
		return txpow;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mNonce.writeDataStream(zOut);
		mMagic.writeDataStream(zOut);
		mChainID.writeDataStream(zOut);
		mParentChainID.writeDataStream(zOut);
		mCustom.writeDataStream(zOut);
		mTimeSecs.writeDataStream(zOut);
		mTxnDifficulty.writeDataStream(zOut);
		mTransaction.writeDataStream(zOut);
		mWitness.writeDataStream(zOut);
		mBurnTransaction.writeDataStream(zOut);
		mBurnWitness.writeDataStream(zOut);
		mBlockNumber.writeDataStream(zOut);
		mParent.writeDataStream(zOut);
		mBlockDifficulty.writeDataStream(zOut);
		
		//The Super parents are efficiently encoded in RLE
		MiniHash old = null;
		int counter=0;
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			MiniHash curr = mSuperParents[i];
			if(old == null) {
				old = curr;
				counter++;
			}else {
				if(old.isExactlyEqual(curr)) {
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
		
		//Write out the TXPOW List
		int len = mTxPowIDList.size();
		MiniNumber ramlen = new MiniNumber(""+len);
		ramlen.writeDataStream(zOut);
		for(MiniHash txpowid : mTxPowIDList) {
			txpowid.writeDataStream(zOut);
		}
		
		//Write out the MMR DB
		mMMRRoot.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mNonce.readDataStream(zIn);
		mMagic.readDataStream(zIn);
		mChainID.readDataStream(zIn);
		mParentChainID.readDataStream(zIn);
		mCustom.readDataStream(zIn);
		mTimeSecs.readDataStream(zIn);
		mTxnDifficulty = MiniHash.ReadFromStream(zIn);
		mTransaction.readDataStream(zIn);
		mWitness.readDataStream(zIn);
		mBurnTransaction.readDataStream(zIn);
		mBurnWitness.readDataStream(zIn);
		mBlockNumber.readDataStream(zIn);
		mParent = MiniHash.ReadFromStream(zIn);
		mBlockDifficulty.readDataStream(zIn);
		
		//And the super parents - RLE
		int tot   = 0;
		while(tot<GlobalParams.MINIMA_CASCADE_LEVELS) {
			MiniByte len   = MiniByte.ReadFromStream(zIn);
			MiniHash sup = MiniHash.ReadFromStream(zIn);
			int count = len.getValue();
			for(int i=0;i<count;i++) {
				mSuperParents[tot++] = sup;
			}
		}
		
		//Read in  the TxPOW list
		mTxPowIDList = new ArrayList<>();
		MiniNumber ramlen = new MiniNumber();
		ramlen.readDataStream(zIn);
		int len = ramlen.getAsInt();
		for(int i=0;i<len;i++) {
			mTxPowIDList.add(MiniHash.ReadFromStream(zIn));
		}
		
		//read in the MMR state..
		mMMRRoot.readDataStream(zIn);
		
		//The ID
		calculateTXPOWID();
	}
	
	/**
	 * Used internally
	 * 
	 * @return
	 */
	public MiniHash getTxPowID() {
		return _mTxPOWID;
	}
	
	public MiniHash getTransID() {
		return _mTransID;
	}
	
	public int getSuperLevel() {
		return _mSuperBlock;
	}
	
	public boolean isBlock() {
		return _mIsBlockPOW;
	}
	
	public boolean isTransaction() {
		return _mIsTxnPOW;
	}
	
	/**
	 * This is only done once at creation. TXPOW structures are immutable.
	 */
	public void calculateTXPOWID() {
		//The TXPOW ID
		_mTxPOWID = Crypto.getInstance().hashObject(this);
		
		//The Transaction ID
		_mTransID = Crypto.getInstance().hashObject(mTransaction);
		
		//Valid Block
		_mIsBlockPOW = false;
		if(_mTxPOWID.isLess(getBlockDifficulty())) {
			_mIsBlockPOW = true;
		}
		
		//Valid Transaction
		_mIsTxnPOW = false;
		if(_mTxPOWID.isLess(getTxnDifficulty()) && !getTransaction().isEmpty()) {
			_mIsTxnPOW = true;
		}	
		
		//What Super Level are we..
		_mSuperBlock = SuperBlockLevels.getSuperLevel(getBlockDifficulty(), _mTxPOWID);
	}
}
