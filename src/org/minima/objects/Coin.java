package org.minima.objects;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMREntryNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Coin implements Streamable {
	
	/**
	 * Normal Outputs don't specify a coinid
	 */
	public static final MiniData COINID_OUTPUT  = new MiniData("0x00");
	
	/**
	 * Floating Inputs - used for ELTOO
	 */
	public static final MiniData COINID_ELTOO  	= new MiniData("0x01");
	
	/**
	 * The GLOBAL UNIQUE CoinID for this coin.
	 * 
	 * This is present for Inputs. Otherwise it is set to 0x00 ( Since it cannot be calculated for the outputs 
	 * as it refers back to the Hash of Itself. ) 
	 * 
	 * It is the Hash of the Transaction hash ( inputs state outputs ) and the output 
	 * num of the coin in that Transaction. This is GLOBALLY Unique.
	 * 
	 * KECCAK ( TXN_HASH | OUTPUT_NUM_IN_TXN ) 
	 */
	MiniData 	mCoinID;
	
	/**
	 * The Address.
	 *  
	 * This is the hash of the Script that controls this coin.
	 */
	MiniData 	mAddress;
	
	/**
	 * The Value of this Coin.
	 */
	MiniNumber 	mAmount;
	
	/**
	 * Tokens are Native in Minima. All inputs and outputs have them. MINIMA the default is 0x00
	 */
	MiniData  mTokenID;

	/**
	 * Floating Input Coins can be attached to any coin with the correct address, amount and tokenid.
	 */
	boolean mFloating = false;

	/**
	 * Output coins can choose to not store the state data and save space - change outputs for instance need no state
	 */
	boolean mStoreState = true;
	
	/**
	 * The State Variables - is empty when an output coin.. 
	 */
	ArrayList<StateVariable> mState = new ArrayList<>();

	/**
	 * The Entry Number in the MMR
	 */
	MMREntryNumber mMMREntryNumber	= MMREntryNumber.ZERO;
	
	/**
	 * Spent or Unspent
	 */
	MiniByte mSpent				= MiniByte.FALSE;
	
	/**
	 * The Block number where this output was created 
	 */
	MiniNumber mBlockCreated	= MiniNumber.ZERO;
	
	/**
	 * Then Token Details 
	 */
	Token mToken = null;
	
	/**
	 * Main Constructor
	 */
	public Coin(MiniData zAddress, MiniNumber zAmount, MiniData zTokenID) {
		this(Coin.COINID_OUTPUT, zAddress, zAmount, zTokenID, false, true);
	}
	
	public Coin(MiniData zCoinID, MiniData zAddress, MiniNumber zAmount, MiniData zTokenID) {
		this(zCoinID, zAddress, zAmount, zTokenID, false, true);
	}
		
	public Coin(MiniData zCoinID, MiniData zAddress, MiniNumber zAmount, MiniData zTokenID, boolean zFloating, boolean zStoreState) {
		mCoinID  = zCoinID;
		mAddress = zAddress;
		mAmount  = zAmount;
		mTokenID = zTokenID;
		mFloating   = zFloating;
		mStoreState = zStoreState;
	}
	
	private Coin() {}
	
	/**
	 * Return the same Coin but with a new CoinID - output coinid are computed after the fact
	 */
	public Coin getSameCoinWithCoinID(MiniData zCoinID) {
		Coin copy = deepCopy();
		copy.resetCoinID(zCoinID);
		return copy;
	}
	
	/**
	 * Floating inputs change the CoinID
	 */
	public void resetCoinID(MiniData zCoinID) {
		mCoinID = zCoinID;
	}
	
	public void resetTokenID(MiniData zTokenID) {
		mTokenID = zTokenID;
	}
	
	public Token getToken() {
		return mToken;
	}
	
	public void setToken(Token zToken) {
		mToken = zToken;
	}
	
	public void setMMREntryNumber(MMREntryNumber zEntryNumber) {
		mMMREntryNumber = zEntryNumber;
	}
	
	public MMREntryNumber getMMREntryNumber() {
		return mMMREntryNumber;
	}
	
	public void setSpent(boolean zSpent) {
		mSpent = new MiniByte(zSpent);
	}
	
	public boolean getSpent() {
		return mSpent.isTrue();
	}
	
	public void setBlockCreated(MiniNumber zBlock) {
		mBlockCreated = zBlock;
	}
	
	public MiniNumber getBlockCreated() {
		return mBlockCreated;
	}
	
	public void setFloating(boolean zFloating) {
		mFloating = zFloating;
	}
	
	public boolean isFloating() {
		return mFloating;
	}
	
	/**
	 * Do we store the state for this coin
	 * @return
	 */
	public boolean storeState() {
		return mStoreState;
	}
	
	public MiniData getCoinID() {
		return mCoinID;
	}
	
	public MiniData getAddress() {
		return mAddress;
	}
	
	public MiniNumber getAmount() {
		return mAmount;
	}

	public MiniData getTokenID() {
		return mTokenID;
	}
	
	public ArrayList<StateVariable> getState(){
		return mState;
	}
	
	public void setState(ArrayList<StateVariable> zCompleteState) {
		mState = zCompleteState;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("coinid", mCoinID.toString());
		
		obj.put("amount", mAmount.toString());
		
		obj.put("address", mAddress.toString());
		
		//Get the MxAddress..
//		String mxaddr = Address.makeMinimaAddress(mAddress);
//		obj.put("mxaddress", mxaddr);
		
		obj.put("tokenid", mTokenID.toString());
		if(mToken == null) {
			obj.put("token", null);
		}else {
			obj.put("token", mToken.toJSON());
			
			//What is the tokenamount..
			MiniNumber tokenamt = getToken().getScaledTokenAmount(getAmount());
			obj.put("tokenamount", tokenamt.toString());
		}
		
		obj.put("floating", mFloating);
		obj.put("storestate", mStoreState);
		
		//Add the state variables
		JSONArray starr = new JSONArray();
		for(StateVariable sv : mState) {
			starr.add(sv.toJSON());
		}
		obj.put("state", starr);
		
		obj.put("mmrentry", mMMREntryNumber.toString());
		obj.put("spent", mSpent.isTrue());
		obj.put("created", mBlockCreated.toString());
		
		return obj;
	}
	
	/**
	 * Get a DEEP copy of this Coin
	 */
	public Coin deepCopy(){
		try {
			//First write transaction out to a byte array
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			dos.close();
			
			//Now read it into a new transaction..
			byte[] coinbytes = baos.toByteArray();
			ByteArrayInputStream bais = new ByteArrayInputStream(coinbytes);
			DataInputStream dis = new DataInputStream(bais);
			
			Coin deepcopy = new Coin();
			deepcopy.readDataStream(dis);
			
			dis.close();
			baos.close();
			
			return deepcopy;
			
		}catch(IOException ioexc) {
			MinimaLogger.log(ioexc);
		}	
		
		return null;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCoinID.writeHashToStream(zOut);
		mAddress.writeHashToStream(zOut);
		mAmount.writeDataStream(zOut);
		mTokenID.writeHashToStream(zOut);
		
		MiniByte.WriteToStream(zOut, mFloating);
		MiniByte.WriteToStream(zOut, mStoreState);
		
		mMMREntryNumber.writeDataStream(zOut);
		mSpent.writeDataStream(zOut);
		mBlockCreated.writeDataStream(zOut);
		
		MiniNumber.WriteToStream(zOut, mState.size());
		for(StateVariable sv : mState) {
			sv.writeDataStream(zOut);
		}
		
		if(mToken == null) {
			MiniByte.WriteToStream(zOut, false);
		}else {
			MiniByte.WriteToStream(zOut, true);
			mToken.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID   		= MiniData.ReadHashFromStream(zIn);
		mAddress  		= MiniData.ReadHashFromStream(zIn);
		mAmount   		= MiniNumber.ReadFromStream(zIn);
		mTokenID  		= MiniData.ReadHashFromStream(zIn);
		
		mFloating   	= MiniByte.ReadFromStream(zIn).isTrue();
		mStoreState 	= MiniByte.ReadFromStream(zIn).isTrue();
		
		mMMREntryNumber = MMREntryNumber.ReadFromStream(zIn);
		mSpent			= MiniByte.ReadFromStream(zIn);
		mBlockCreated	= MiniNumber.ReadFromStream(zIn);
		
		mState		= new ArrayList<>();
		int len 	= MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mState.add(StateVariable.ReadFromStream(zIn));
		}
		
		//Is there a token descriptor
		mToken = null;
		if(MiniByte.ReadFromStream(zIn).isTrue()) {
			mToken = Token.ReadFromStream(zIn);
		}
	}
	
	public static Coin ReadFromStream(DataInputStream zIn) throws IOException {
		Coin coin = new Coin();
		coin.readDataStream(zIn);
		return coin;
	}
}
