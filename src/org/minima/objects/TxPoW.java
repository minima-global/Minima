/**
 * 
 */
package org.minima.objects;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
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
	private String _mTxPOWIDStr   		= "0x00";
	private MiniData _mTxPOWID      	= MiniData.ZERO_TXPOWID;
	protected boolean _mIsBlockPOW  	= false;
	protected boolean _mIsTxnPOW    	= false;
	protected int     _mSuperBlock  	= 0;
	protected long     _mTxPoWSize  	= 0;
	protected BigDecimal _mBlockWeight 	= BigDecimal.ZERO;
	
	/**
	 * Test Parameters
	 */
	protected boolean 	  mIsTesting 		= false;
	protected MiniNumber _mTestBlockNumber  = MiniNumber.ZERO;
	protected boolean    _mTestIsBlock  	= true;
	protected boolean    _mTestIsTxn  		= false;
	protected MiniData   _mTestParent  		= MiniData.ZERO_TXPOWID;
	protected ArrayList<String> mTestTransactions = new ArrayList<>();
	
	/**
	 * TEST Constructor
	 */
	public TxPoW(String zTxPoWID, int zBlock, int zWeight) {
		this(zTxPoWID, zBlock, zWeight, true, "0x00", false);
	}
	
	public TxPoW(String zTxPoWID, int zBlock, int zWeight, boolean zIsBlock, String zParent, boolean zIsTransaction) {
		mIsTesting 			= true;
		_mTxPOWIDStr 		= zTxPoWID;
		_mTestBlockNumber 	= new MiniNumber(zBlock);
		_mBlockWeight		= new BigDecimal(""+zWeight);
		_mTestIsBlock 		= zIsBlock;
		_mTestIsTxn 		= zIsTransaction;
		_mTestParent 		= new MiniData(zParent);
		
		mHeader				= new TxHeader();
	}
	
	//TEST FUNCTION
	public void addTestTransaction(String zTxPoWID) {
		mTestTransactions.add(zTxPoWID);
	}
	
	public ArrayList<String> getTransactions(){
		if(mIsTesting) {
			return mTestTransactions;
		}
		
		ArrayList<String> ret = new ArrayList<>();
		
		//if no body just return an empty list
		if(!hasBody()) {
			return ret;	
		}
		
		for(MiniData blk : mBody.mTxPowIDList) {
			ret.add(blk.to0xString());
		}
		
		return ret;
	}
	
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
	
	public boolean isMonotonic() {
		
		boolean transmon 	= mBody.mTransaction.isCheckedMonotonic();
		boolean burnmon 	= true;
		if(!mBody.mBurnTransaction.isEmpty()) {
			burnmon = mBody.mBurnTransaction.isCheckedMonotonic();
		}
		
		return transmon && burnmon;
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
	
	public void setNonce(MiniNumber zNonce) {
		mHeader.mNonce = zNonce;
	}
	
	public MiniNumber getNonce() {
		return mHeader.mNonce;
	}
	
	public MiniData getChainID() {
		return mHeader.mChainID;
	}
	
	public Magic getMagic() {
		return mHeader.mMagic;
	}
	
	public void setMagic(Magic zMagic) {
		mHeader.mMagic = zMagic;
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
	
	public void setBurnTransaction(Transaction zTran) {
		mBody.mBurnTransaction = zTran;
	}
	
	public void setBurnWitness(Witness zWitness) {
		mBody.mBurnWitness = zWitness;
	}
	
	public Witness getWitness() {
		return mBody.mWitness;
	}
	
	public Witness getBurnWitness() {
		return mBody.mBurnWitness;
	}
	
	public void addBlockTxPOW(MiniData zTxPOWID) {
		mBody.mTxPowIDList.add(zTxPOWID);
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
		if(mIsTesting) {
			return _mTestParent;
		}
		
		return getSuperParent(0);
	}

	public void setSuperParent(int zLevel, MiniData zSuperParent) {
		mHeader.mSuperParents[zLevel] = zSuperParent;
	}
	
	public MiniData getSuperParent(int zLevel) {
		return mHeader.mSuperParents[zLevel];
	}
	
	public MiniNumber getBurn() {
		return getTransaction().getBurn().add(getBurnTransaction().getBurn());
	}
	
	public void setTimeMilli() {
		setTimeMilli(new MiniNumber(System.currentTimeMillis()));
	}
	
	public void setTimeMilli(MiniNumber zMilli) {
		mHeader.mTimeMilli = zMilli;
	}
	
	public MiniNumber getTimeMilli() {
		return mHeader.mTimeMilli;
	}
	
	public void setBlockNumber(MiniNumber zBlockNum) {
		mHeader.mBlockNumber = zBlockNum;
	}
	
	public MiniNumber getBlockNumber() {
		if(mIsTesting) {
			return _mTestBlockNumber;
		}
		
		return mHeader.mBlockNumber;
	}
	
	public MiniData getMMRRoot() {
		return mHeader.mMMRRoot;
	}
	
	public void setMMRRoot(MiniData zRoot) {
		mHeader.mMMRRoot = zRoot;
	}
	
	public MiniNumber getMMRTotal() {
		return mHeader.mMMRTotal;
	}
	
	public void setMMRTotal(MiniNumber zTotal) {
		mHeader.mMMRTotal= zTotal;
	}
	
	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		if(mIsTesting) {
			txpow.put("txpowid", _mTxPOWIDStr);
			txpow.put("parentid", _mTestParent.to0xString());
			txpow.put("blocknumber", _mTestBlockNumber);
			txpow.put("weight", _mBlockWeight);
			txpow.put("isblock", _mTestIsBlock);
			txpow.put("istxn", _mTestIsTxn);
			
		}else {
			txpow.put("txpowid", _mTxPOWID.toString());
			txpow.put("isblock", _mIsBlockPOW);
			txpow.put("istransaction", _mIsTxnPOW);
			txpow.put("superblock", _mSuperBlock);
			txpow.put("size", getSizeinBytes());
			
			txpow.put("header", mHeader.toJSON());
			
			txpow.put("hasbody", hasBody());
			if(hasBody()) {
				txpow.put("body", mBody.toJSON());	
			}else {
				txpow.put("body", "null");
			}
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
	
	public static TxPoW ReadFromStream(DataInputStream zIn) throws IOException {
		TxPoW txp = new TxPoW();
		txp.readDataStream(zIn);
		return txp;
	}
	
	
	/**
	 * Get a DEEP copy of this TxPoW
	 */
	public TxPoW deepCopy(){
		try {
			//First write transaction out to a byte array
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			dos.close();
			
			//Now read it into a new transaction..
			byte[] txpowbytes = baos.toByteArray();
			ByteArrayInputStream bais = new ByteArrayInputStream(txpowbytes);
			DataInputStream dis = new DataInputStream(bais);
			
			TxPoW deepcopy = new TxPoW();
			deepcopy.readDataStream(dis);
			
			dis.close();
			baos.close();
			
			return deepcopy;
			
		}catch(IOException ioexc) {
			MinimaLogger.log(ioexc);
		}	
		
		return null;
	}
	
	/**
	 * Convert a MiniData version into a TxPoW
	 */
	public static TxPoW convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		TxPoW txpow = null;
		
		try {
			//Convert data into a TxPoW
			txpow = TxPoW.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return txpow;
	}
	
	/**
	 * Compute the transaction hash for both
	 */
	public void calculateTransactionID() {
		mBody.mTransaction.calculateTransactionID();
		mBody.mBurnTransaction.calculateTransactionID();
	}
	
	/**
	 * Used internally
	 */
	public String getTxPoWID() {
		return _mTxPOWIDStr;
	}
	
	public MiniData getTxPoWIDData() {
		return _mTxPOWID;
	}
	
	public int getSuperLevel() {
		return _mSuperBlock;
	}
	
	public boolean isBlock() {
		if(mIsTesting) {
			return _mTestIsBlock;
		}
		
		return _mIsBlockPOW;
	}
	
	public BigDecimal getWeight() {
		return _mBlockWeight;
	}
	
	public boolean isTransaction() {
		if(mIsTesting) {
			return _mTestIsTxn;
		}
		
		if(!hasBody()) {
			return false;
		}
		
		return !getTransaction().isEmpty();
	}
	
	public long getSizeinBytes() {
		return _mTxPoWSize;
	}
	
	public long getSizeinBytesWithoutBlockTxns() {
		
		long txns = 32 * getBlockTransactions().size();
		
		return _mTxPoWSize - txns;
	}
	
	/**
	 * This is only done once at creation. TXPOW structures are immutable.
	 */
	public void calculateTXPOWID() {
		//The TXPOW ID
		_mTxPOWID 		= Crypto.getInstance().hashObject(mHeader);
		_mTxPOWIDStr 	= _mTxPOWID.to0xString(); 
		
		//Valid Block
		_mIsBlockPOW = _mTxPOWID.isLess(getBlockDifficulty());
		_mBlockWeight = BigDecimal.ZERO;
		if(_mIsBlockPOW) {
//			BigDecimal max = new BigDecimal(Crypto.MAX_VAL);
//			BigDecimal blk = new BigDecimal(getBlockDifficulty().getDataValue());
//			
//			//Divide..
//			BigDecimal weight = max.divide(blk, MiniNumber.MATH_CONTEXT);
//			MinimaLogger.log("Weight : "+_mTxPOWIDStr+" "+weight);
//			
//			//What is the weight..
//			_mBlockWeight = weight.toBigInteger();
			
			BigDecimal blkweightdec = new BigDecimal(getBlockDifficulty().getDataValue());
			_mBlockWeight 			= Crypto.MAX_VALDEC.divide(blkweightdec, MathContext.DECIMAL32);
		}
		
		//The Transaction ID
		_mIsTxnPOW = false;
		if(hasBody()) {
			//Whats the Transaction ID
			calculateTransactionID();
		
			//Valid Transaction
			if(_mTxPOWID.isLess(getTxnDifficulty()) && !getTransaction().isEmpty()) {
				_mIsTxnPOW = true;
			}
		}
		
		//What Super Level are we..
		_mSuperBlock = getSuperLevel(getBlockDifficulty(), _mTxPOWID);
		if(_mSuperBlock>=GlobalParams.MINIMA_CASCADE_LEVELS) {
			_mSuperBlock = GlobalParams.MINIMA_CASCADE_LEVELS-1;
		}
		
		//What size are we..
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos       = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			baos.flush();
			
			//Get the Size
			_mTxPoWSize = baos.toByteArray().length;
			
			dos.close();
			baos.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
	
	/**
	 * This calculates the Log2 of the Difficulty and TxPoW unit..
	 */
	private int getSuperLevel(MiniData zBlockDifficulty, MiniData zTxPoWID) {
		//What is the 
		BigInteger quot = zBlockDifficulty.getDataValue().divide(zTxPoWID.getDataValue());
		
		//Use a clever trick.. the bit length
		int sup = quot.bitLength()-1;
		
		return sup;
	}
}
