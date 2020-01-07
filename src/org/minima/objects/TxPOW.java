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

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.SuperBlockLevels;
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
	private MiniNumber 	mNonce	= new MiniNumber();
	
	/**
	 * TimeMilli  
	 */
	private MiniNumber 	mTimeMilli= new MiniNumber();
	
	/**
	 * The Difficulty for this TXPOW to be valid.
	 * 
	 * This is calculated 2^n.. so level 255, the max, is 2^255
	 * 
	 * The amount of work (Proof-of-work) that has gone into it.
	 */
	private MiniByte 	mTxnDifficulty = new MiniByte();
	
	/**
	 * The Transaction the user is trying to send
	 */
	private Transaction	mTransaction = new Transaction();
	
	/**
	 * The Witness data for the Transaction
	 */
	private Witness		mWitness = new Witness();
	
	/**
	 * The Block Number
	 */
	private MiniNumber  mBlockNumber = new MiniNumber();
	
	/**
	 * The previous TXPOW Block
	 */
	private MiniData32  mParent = new MiniData32();
	
	/**
	 * The BASE Block Difficulty
	 */
	private MiniByte mBlockDifficulty = new MiniByte();
	
	/**
	 * The list of the current TX-POWs the user 
	 * knows about that are not yet in the this chain.
	 */
	private ArrayList<MiniData32> mTxPowIDList;
	
	/**
	 * A list of all the parent blocks at all the Super Block Levels..
	 */
	public static final int SUPERPARENT_NUM = 256;
	public MiniData32[] mSuperParents = new MiniData32[SUPERPARENT_NUM];
	
	/**
	 * The MMR Root!
	 */
	public MiniData32 mMMRRoot = new MiniData32();
	
	/**
	 * A Random Magicx number so that everyone is working on a different TxPOW (since there is no coinbase..)
	 */
	public MiniData32 mMagic = MiniData32.getRandomData();
	
	/**
	 * These are used internally ONLY
	 */
	private MiniData32 _mTxPOWID = new MiniData32();
	
	protected boolean _mIsBlockPOW;
	protected boolean _mIsTxnPOW;
	protected int     _mSuperBlock;
	
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
		for(int i=0;i<SUPERPARENT_NUM;i++) {
			mSuperParents[i] = new MiniData32();
		}
	}
	
	public void setNonce(MiniNumber zNonce) {
		mNonce = zNonce;
	}
	
	public void setTxDifficulty(int zDifficulty) {
		mTxnDifficulty = new MiniByte(zDifficulty);
	}
	
	public int getTxnDifficulty() {
		return mTxnDifficulty.getValue();
	}
	
	public void setTimeMilli(MiniNumber zMilli) {
		mTimeMilli = zMilli;
	}
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public void setTransaction(Transaction zTran) {
		mTransaction = zTran;
	}
	
	public void addBlockTxPOW(TxPOW zTxPOW) {
		mTxPowIDList.add(zTxPOW.getTxPowID());
	}
	
	public ArrayList<MiniData32> getBlockTxns(){
		return mTxPowIDList;
	}
	
	public int getBlockDifficulty() {
		return mBlockDifficulty.getValue();
	}
	
	public void setBlockDifficulty(int zBlockDifficulty) {
		mBlockDifficulty = new MiniByte(zBlockDifficulty);
	}
	
	public void setParent(MiniData32 zData) {
		mParent = zData;
	}
	
	public MiniData32 getParentID() {
		return mParent;
	}
	
	public MiniNumber getTimeMilli() {
		return mTimeMilli;
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
	
	public MiniData32 getMMRRoot() {
		return mMMRRoot;
	}
	
	public void setMMRRoot(MiniData32 zRoot) {
		mMMRRoot = zRoot;
	}
	
	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("block", mBlockNumber.toString());
		txpow.put("isblock", _mIsBlockPOW);
		txpow.put("txpowid", _mTxPOWID.toString());
		txpow.put("parent", mParent.toString());
		txpow.put("blkdiff", mBlockDifficulty);
		txpow.put("txndiff", mTxnDifficulty);
		txpow.put("txn", mTransaction.toJSON());
		txpow.put("witness", mWitness.toJSON());
		txpow.put("witness", mWitness.toJSON());
		
		//Need to make it into a JSON array
		txpow.put("txnlist", mTxPowIDList);
		
		txpow.put("magic", mMagic.toString());
		txpow.put("nonce", mNonce.toString());
		txpow.put("mmr", mMMRRoot.toString());
		
		txpow.put("timemilli", mTimeMilli.toString());
		txpow.put("date", new Date(mTimeMilli.getAsLong()).toString());
		
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
		mTimeMilli.writeDataStream(zOut);
		mTxnDifficulty.writeDataStream(zOut);
		mTransaction.writeDataStream(zOut);
		mWitness.writeDataStream(zOut);
		mBlockNumber.writeDataStream(zOut);
		mParent.writeDataStream(zOut);
		mBlockDifficulty.writeDataStream(zOut);
		
		//The Super parents are efficiently encoded in RLE
		MiniData32 old = null;
		int counter=0;
		for(int i=0;i<SUPERPARENT_NUM;i++) {
			MiniData32 curr = mSuperParents[i];
			if(old == null) {
				old = curr;
				counter++;
			}else {
				if(old.isExactlyEqual(curr)) {
					counter++;
					//Is this the last one..
					if(i==SUPERPARENT_NUM-1) {
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
		for(MiniData32 txpowid : mTxPowIDList) {
			txpowid.writeDataStream(zOut);
		}
		
		//Write out the MMR DB
		mMMRRoot.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mNonce.readDataStream(zIn);
		mMagic.readDataStream(zIn);
		mTimeMilli.readDataStream(zIn);
		mTxnDifficulty = MiniByte.ReadFromStream(zIn);
		mTransaction.readDataStream(zIn);
		mWitness.readDataStream(zIn);
		mBlockNumber.readDataStream(zIn);
		mParent = MiniData32.ReadFromStream(zIn);
		mBlockDifficulty.readDataStream(zIn);
		
		//And the super parents - RLE
		int tot   = 0;
		while(tot<SUPERPARENT_NUM) {
			MiniByte len   = MiniByte.ReadFromStream(zIn);
			MiniData32 sup = MiniData32.ReadFromStream(zIn);
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
			mTxPowIDList.add(MiniData32.ReadFromStream(zIn));
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
	public MiniData32 getTxPowID() {
		return _mTxPOWID;
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
		
		Difficulty blkdiff = new Difficulty(getBlockDifficulty()); 
		Difficulty txndiff = new Difficulty(getTxnDifficulty());
		
		_mIsBlockPOW = false;
		if(blkdiff.isOK(_mTxPOWID)){
			_mIsBlockPOW = true;
		}
		
		_mIsTxnPOW = false;
		if(txndiff.isOK(_mTxPOWID) && !getTransaction().isEmpty()) {
			_mIsTxnPOW = true;
		}	
		
		_mSuperBlock = SuperBlockLevels.getSupers().getSuperBlockLevel(_mTxPOWID);
		if(_mSuperBlock>=TxPOW.SUPERPARENT_NUM) {
			_mSuperBlock = TxPOW.SUPERPARENT_NUM-1;
		}
	}
}
