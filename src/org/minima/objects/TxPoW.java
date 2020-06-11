/**
 * 
 */
package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
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
public class TxPoW implements Streamable {
	
	/**
	 * The TxPoW Header - what is hashed and kept if becomes a cascade block
	 * 
	 * This includes the super parent blocks 
	 */
	TxHeader mHeader;
	
	/**
	 * The TxPoW body where the actual information is kept
	 * 
	 * This is discarded and set to NULL if a cascading node..
	 * 
	 * The transactions, witness, signatures and txnlist.. not kept in the long run..
	 */
	TxBody mBody;
		
	/**
	 * These are used internally ONLY
	 */
	private MiniData _mTxPOWID      = new MiniData("0x00");
	private MiniData _mTransID      = new MiniData("0x00");
	protected boolean _mIsBlockPOW  = false;
	protected boolean _mIsTxnPOW    = false;
	protected int     _mSuperBlock  = 0;
	
	/**
	 * Main Constructor
	 */
	public TxPoW() {
		mHeader = new TxHeader();
		mBody   = new TxBody();
	}
	
	public TxHeader getTxHeader() {
		return mHeader;
	}
	
	public MiniData getTxHeaderBodyHash() {
		return mHeader.getBodyHash();
	}
	
	public void setHeaderBodyHash() {
		mHeader.mTxBodyHash = Crypto.getInstance().hashObject(mBody);
	}
	
	public TxBody getTxBody() {
		return mBody;	
	}
	
	public boolean hasBody() {
		return mBody != null;
	}
	
	public void clearBody() {
		mBody = null;
	}
	
	public void setNonce(MiniInteger zNonce) {
		mHeader.mNonce = zNonce;
	}
	
	public MiniInteger getNonce() {
		return mHeader.mNonce;
	}
	
	public void setChainID(MiniData zChainID) {
		mHeader.mChainID = zChainID;
	}
	
	public void setParentChainID(MiniData zChainID) {
		mHeader.mParentChainID = zChainID;
	}
	
	public MiniData getChainID() {
		return mHeader.mChainID;
	}
	
	public MiniData getParentChainID() {
		return mHeader.mParentChainID;
	}
		
	public void setTxDifficulty(MiniData zDifficulty) {
		mBody.mTxnDifficulty = zDifficulty;
	}
	
	public MiniData getTxnDifficulty() {
		return mBody.mTxnDifficulty;
	}
	
	public Transaction getTransaction() {
		return mBody.mTransaction;
	}
	
	public Transaction getBurnTransaction() {
		return mBody.mBurnTransaction;
	}
	
	public void setTransaction(Transaction zTran) {
		mBody.mTransaction = zTran;
	}
	
	public void setWitness(Witness zWitness) {
		mBody.mWitness = zWitness;
	}
	
	public Witness getWitness() {
		return mBody.mWitness;
	}
	
	public Witness getBurnWitness() {
		return mBody.mBurnWitness;
	}
	
	public void addBlockTxPOW(TxPoW zTxPOW) {
		mBody.mTxPowIDList.add(zTxPOW.getTxPowID());
	}
	
	public ArrayList<MiniData> getBlockTransactions(){
		//if no body just return an empty list
		if(!hasBody()) {
			return new ArrayList<MiniData>();	
		}
		
		return mBody.mTxPowIDList;
	}
	
	public MiniData getBlockDifficulty() {
		return mHeader.mBlockDifficulty;
	}
	
	public void setBlockDifficulty(MiniData zBlockDifficulty) {
		mHeader.mBlockDifficulty = zBlockDifficulty;
	}
	
	public MiniData getParentID() {
		return getSuperParent(0);
	}

	public void setSuperParent(int zLevel, MiniData zSuperParent) {
		mHeader.mSuperParents[zLevel] = zSuperParent;
	}
	
	public MiniData getSuperParent(int zLevel) {
		return mHeader.mSuperParents[zLevel];
	}
	
	public void setTimeSecs(MiniNumber zSecs) {
		mHeader.mTimeSecs = zSecs;
	}
	
	public MiniNumber getTimeSecs() {
		return mHeader.mTimeSecs;
	}
	
	public void setTimeMilli(MiniNumber zMilli) {
		mHeader.mTimeSecs = zMilli.divRoundDown(MiniNumber.THOUSAND);
	}
	
	public MiniNumber getTimeMilli() {
		return mHeader.mTimeSecs.mult(MiniNumber.THOUSAND);
	}
	
	public void setBlockNumber(MiniNumber zBlockNum) {
		mHeader.mBlockNumber = zBlockNum;
	}
	
	public MiniNumber getBlockNumber() {
		return mHeader.mBlockNumber;
	}
	
	public MiniData getMagic() {
		return mBody.mMagic;
	}
	
	public MiniData getMMRRoot() {
		return mHeader.mMMRRoot;
	}
	
	public void setMMRRoot(MiniData zRoot) {
		mHeader.mMMRRoot = zRoot;
	}
	
	public MMRSumNumber getMMRTotal() {
		return mHeader.mMMRTotal;
	}
	
	public void setMMRTotal(MMRSumNumber zTotal) {
		mHeader.mMMRTotal= zTotal;
	}
	
	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("isblock", _mIsBlockPOW);
		txpow.put("istransaction", _mIsTxnPOW);
		txpow.put("txpowid", _mTxPOWID.toString());
		txpow.put("superblock", _mSuperBlock);
		
		txpow.put("header", mHeader.toJSON());
		
		txpow.put("hasbody", hasBody());
		if(hasBody()) {
			txpow.put("body", mBody.toJSON());	
		}else {
			txpow.put("body", "null");
		}
		
		return txpow;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//There is always the Header
		mHeader.writeDataStream(zOut);
		
		//Is there a body..
		if(mBody==null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mBody.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mHeader.readDataStream(zIn);
		
		if(MiniByte.ReadFromStream(zIn).isTrue()) {
			mBody.readDataStream(zIn);
		}else {
			mBody = null;
		}
		
		//The TxPoWID / Super levels.. etc..
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
		_mTxPOWID = Crypto.getInstance().hashObject(mHeader);
		
		//Valid Block
		_mIsBlockPOW = _mTxPOWID.isLess(getBlockDifficulty());
		
		//The Transaction ID
		_mIsTxnPOW = false;
		if(hasBody()) {
			//Whats the Transaction ID
			_mTransID = Crypto.getInstance().hashObject(mBody.mTransaction);
		
			//Valid Transaction
			if(_mTxPOWID.isLess(getTxnDifficulty()) && !getTransaction().isEmpty()) {
				_mIsTxnPOW = true;
			}
		}
		
		//What Super Level are we..
		_mSuperBlock = SuperBlockLevels.getSuperLevel(getBlockDifficulty(), _mTxPOWID);
	}
}
