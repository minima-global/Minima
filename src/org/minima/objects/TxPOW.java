/**
 * 
 */
package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import org.minima.GlobalParams;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
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
	private MiniInteger mNonce	= new MiniInteger(0);
	
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
	
	private MiniData 	mTxnDifficulty = new MiniData();
	
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
	private MiniData  mParent = new MiniData();
	
	/**
	 * The BASE Block Difficulty
	 */
	private MiniData mBlockDifficulty = new MiniData();
	
	/**
	 * The list of the current TX-POWs the user 
	 * knows about that are not yet in the this chain.
	 */
	private ArrayList<MiniData> mTxPowIDList;
	
	/**
	 * A list of all the parent blocks at all the Super Block Levels..
	 */
	public MiniData[] mSuperParents = new MiniData[GlobalParams.MINIMA_CASCADE_LEVELS];
	
	/**
	 * The MMR Root!
	 */
	public MiniData mMMRRoot = new MiniData();
	
	/**
	 * The Total Sum Of All coins in the system
	 */
	public MMRSumNumber mMMRTotal = MMRSumNumber.ZERO;
	
	/**
	 * A Random Magic number so that everyone is working on a different TxPOW in the pulse 
	 * (since there is no coinbase..)
	 */
	public MiniData mMagic = MiniData.getRandomData(32);
	
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
	 * A Custom Hash. Can be anything the user wants..
	 */
	public MiniData mCustom = new MiniData("0x00");
	
	/**
	 * These are used internally ONLY
	 */
	private MiniData _mTxPOWID = new MiniData("0x00");
	private MiniData _mTransID = new MiniData("0x00");
	
	protected boolean _mIsBlockPOW  = false;
	protected boolean _mIsTxnPOW    = false;
	protected int     _mSuperBlock  = 0;
	
	/**
	 * Main Constructor
	 */
	public TxPOW() {
		mTxPowIDList = new ArrayList<>();
		
		//Super Block Levels..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			mSuperParents[i] = new MiniData();
		}
	}
	
	public void setNonce(MiniInteger zNonce) {
		mNonce = zNonce;
	}
	
	public MiniInteger getNonce() {
		return mNonce;
	}
	
	public void setChainID(MiniData zChainID) {
		mChainID = zChainID;
	}
	
	public void setParentChainID(MiniData zChainID) {
		mParentChainID = zChainID;
	}
	
	public MiniData getChainID() {
		return mChainID;
	}
	
	public MiniData getParentChainID() {
		return mParentChainID;
	}
	
	public void setCustom(MiniData zCustom) {
		mCustom = zCustom;
	}
	
	public MiniData getCustom() {
		return mCustom;
	}
	
	public void setTxDifficulty(MiniData zDifficulty) {
		mTxnDifficulty = zDifficulty;
	}
	
	public MiniData getTxnDifficulty() {
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
	
	public ArrayList<MiniData> getBlockTxns(){
		return mTxPowIDList;
	}
	
	public MiniData getBlockDifficulty() {
		return mBlockDifficulty;
	}
	
	public void setBlockDifficulty(MiniData zBlockDifficulty) {
		mBlockDifficulty = zBlockDifficulty;
	}
	
	public void setParent(MiniData zData) {
		mParent = zData;
	}
	
	public MiniData getParentID() {
		return mParent;
	}
	
	public MiniData getSuperParent(int zLevel) {
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
	
	public MiniData getMMRRoot() {
		return mMMRRoot;
	}
	
	public void setMMRRoot(MiniData zRoot) {
		mMMRRoot = zRoot;
	}
	
	public MMRSumNumber getMMRTotal() {
		return mMMRTotal;
	}
	
	public void setMMRTotal(MMRSumNumber zTotal) {
		mMMRTotal= zTotal;
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
		txpow.put("txnid", getTransID().to0xString());
		txpow.put("witness", mWitness.toJSON());
		
		//The BURN transaction.. normally empty
		txpow.put("burntxn", mBurnTransaction.toJSON());
		txpow.put("burnwitness", mBurnWitness.toJSON());
		
		//Need to make it into a JSON array
		JSONArray txns = new JSONArray();
		for(MiniData txn : mTxPowIDList) {
			txns.add(txn.to0xString());
		}
		txpow.put("txnlist", txns);
		
		txpow.put("magic", mMagic.toString());
		txpow.put("chainid", mChainID.toString());
		txpow.put("parentchainid", mParentChainID.toString());
		txpow.put("custom", mCustom.toString());
		txpow.put("nonce", mNonce.toString());
		
		txpow.put("mmr", mMMRRoot.toString());
		txpow.put("total", mMMRTotal.toString());
		
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
		
		//Write out the TXPOW List
		int len = mTxPowIDList.size();
		MiniNumber ramlen = new MiniNumber(""+len);
		ramlen.writeDataStream(zOut);
		for(MiniData txpowid : mTxPowIDList) {
			txpowid.writeDataStream(zOut);
		}
		
		//Write out the MMR DB
		mMMRRoot.writeDataStream(zOut);
		mMMRTotal.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mNonce          = MiniInteger.ReadFromStream(zIn);
		mMagic          = MiniData.ReadFromStream(zIn);
		mChainID        = MiniData.ReadFromStream(zIn);
		mParentChainID  = MiniData.ReadFromStream(zIn);
		mCustom         = MiniData.ReadFromStream(zIn);
		mTimeSecs       = MiniNumber.ReadFromStream(zIn);
		mTxnDifficulty  = MiniData.ReadFromStream(zIn);
		
		mTransaction.readDataStream(zIn);
		mWitness.readDataStream(zIn);
		mBurnTransaction.readDataStream(zIn);
		mBurnWitness.readDataStream(zIn);
		
		mBlockNumber    = MiniNumber.ReadFromStream(zIn);
		mParent         = MiniData.ReadFromStream(zIn);
		mBlockDifficulty= MiniData.ReadFromStream(zIn);
		
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
		
		//Read in  the TxPOW list
		mTxPowIDList = new ArrayList<>();
		MiniNumber ramlen = MiniNumber.ReadFromStream(zIn);
		int len = ramlen.getAsInt();
		for(int i=0;i<len;i++) {
			mTxPowIDList.add(MiniData.ReadFromStream(zIn));
		}
		
		//read in the MMR state..
		mMMRRoot  = MiniData.ReadFromStream(zIn);
		mMMRTotal = MMRSumNumber.ReadFromStream(zIn);
		
		//The ID
		calculateTXPOWID();
	}
	
	/**
	 * Used internally
	 * 
	 * @return
	 */
	public MiniData getTxPowID() {
		return _mTxPOWID;
	}
	
	public MiniData getTransID() {
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
		
		//Is this the Genesis..
//		if(getBlockNumber().isEqual(MiniNumber.ZERO) && _mTxPOWID.isExactlyEqual(SuperBlockLevels.GENESIS_HASH)){
//			_mSuperBlock = 20;
////			System.out.println("SuperBlock set : "+_mSuperBlock+" "+_mTxPOWID);
//		}
		
	}
}
