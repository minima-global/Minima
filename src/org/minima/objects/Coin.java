package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Coin implements Streamable {
	
	/**
	 * Outputs don't specify a coinid
	 */
	public static final MiniHash MINIMA_TOKENID = new MiniHash("0x00");
	
	public static final MiniHash COINID_OUTPUT  = new MiniHash("0x00");
	
	public static final MiniHash TOKENID_CREATE = new MiniHash("0xFF");
	
	/**
	 * The GLOBAL UNIQUE CoinID for this coin.
	 * 
	 * This is present for Inputs. Otherwise it is set to 0x00 ( Since it cannot be calculated for the outputs 
	 * as it refers back to the Hash of Itself. ) 
	 * 
	 * It is the Hash of the Transaction hash ( inputs outputs ) and the output 
	 * num of the coin in that Transaction. This is GLOBALLY Unique.
	 * 
	 * SHA3 ( TXN_HASH | OUTPUT_NUM_IN_TXN ) 
	 */
	MiniHash 	mCoinID;
	
	/**
	 * The RamAddress.
	 *  
	 * This is the hash of the RamScript that controls this coin. This is ALWAYS present
	 */
	MiniHash 	mAddress;
	
	/**
	 * The Value of this Coin. This is ALWAYS present
	 */
	MiniNumber 	mAmount;
	
	/**
	 * Tokens are Native in Minima. All inputs and outputs have them. MINIMA the default is 0x00
	 */
	MiniHash  mTokenID;

	/**
	 * Floating Input Coins can be attached to any coin with the correct address and tokenid and AT LEAST the amount.
	 */
	boolean mFloating = false;

	/**
	 * Outputs can be designated as REMAINDERS for all the value remaining of a certain tokenid.. should the floating input change it.
	 */
	boolean mRemainder = false;
	
	/**
	 * Main Constructor
	 */
	public Coin(MiniHash zCoinID, MiniHash zAddress, MiniNumber zAmount, MiniHash zTokenID) {
		this(zCoinID, zAddress, zAmount, zTokenID, false, false);
	}
		
	public Coin(MiniHash zCoinID, MiniHash zAddress, MiniNumber zAmount, MiniHash zTokenID, boolean zFloating, boolean zRemainder) {
		mCoinID  = zCoinID;
		mAddress = zAddress;
		mAmount  = zAmount;
		mTokenID = zTokenID;
		
		mFloating  = zFloating;
		mRemainder = zRemainder;
	}
	
	private Coin() {}
	
	public void setFloating(boolean zFloating) {
		mFloating = zFloating;
	}
	
	public boolean isFloating() {
		return mFloating;
	}
	
	public void setRemainder(boolean zRemainder) {
		mRemainder = zRemainder;
	}
	
	public boolean isRemainder() {
		return mRemainder;
	}
	
	/**
	 * Floating inputs change the CoinID
	 */
	public void resetCoinID(MiniHash zCoinID) {
		mCoinID = zCoinID;
	}
	
	/**
	 * Floating inputs or Remainder Outputs change the Amount
	 */
	public void resetAmount(MiniNumber zAmount) {
		mAmount = zAmount;
	}
	
	
	public MiniHash getCoinID() {
		return mCoinID;
	}
	
	public MiniHash getAddress() {
		return mAddress;
	}
	
	public MiniNumber getAmount() {
		return mAmount;
	}

	public MiniHash getTokenID() {
		return mTokenID;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("coinid", mCoinID.toString());
		obj.put("address", mAddress.toString());
		obj.put("amount", mAmount.toString());
		obj.put("tokenid", mTokenID.toString());
		obj.put("floating", mFloating);
		
		return obj;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCoinID.writeDataStream(zOut);
		mAddress.writeDataStream(zOut);
		mAmount.writeDataStream(zOut);
		mTokenID.writeDataStream(zOut);
		
		if(mFloating) {
			MiniByte.TRUE.writeDataStream(zOut);
		}else {
			MiniByte.FALSE.writeDataStream(zOut);
		}
		
		if(mRemainder) {
			MiniByte.TRUE.writeDataStream(zOut);
		}else {
			MiniByte.FALSE.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID   = MiniHash.ReadFromStream(zIn);
		mAddress  = MiniHash.ReadFromStream(zIn);
		mAmount   = MiniNumber.ReadFromStream(zIn);
		mTokenID  = MiniHash.ReadFromStream(zIn);
		
		mFloating  = MiniByte.ReadFromStream(zIn).isTrue();
		mRemainder = MiniByte.ReadFromStream(zIn).isTrue();
	}
	
	public static Coin ReadFromStream(DataInputStream zIn) throws IOException {
		Coin coin = new Coin();
		coin.readDataStream(zIn);
		return coin;
	}
}
