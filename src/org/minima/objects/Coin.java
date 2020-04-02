package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Coin implements Streamable {
	
	/**
	 * Outputs don't specify a coinid
	 */
	public static final MiniData MINIMA_TOKENID = new MiniData("0x00");
	public static final MiniData COINID_OUTPUT  = new MiniData("0x00");
	public static final MiniData TOKENID_CREATE = new MiniData("0xFF");
	
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
	MiniData 	mCoinID;
	
	/**
	 * The RamAddress.
	 *  
	 * This is the hash of the RamScript that controls this coin. This is ALWAYS present
	 */
	MiniData 	mAddress;
	
	/**
	 * The Value of this Coin. This is ALWAYS present
	 */
	MiniNumber 	mAmount;
	
	/**
	 * Tokens are Native in Minima. All inputs and outputs have them. MINIMA the default is 0x00
	 */
	MiniData  mTokenID;

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
	public Coin(MiniData zCoinID, MiniData zAddress, MiniNumber zAmount, MiniData zTokenID) {
		this(zCoinID, zAddress, zAmount, zTokenID, false, false);
	}
		
	public Coin(MiniData zCoinID, MiniData zAddress, MiniNumber zAmount, MiniData zTokenID, boolean zFloating, boolean zRemainder) {
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
	public void resetCoinID(MiniData zCoinID) {
		mCoinID = zCoinID;
	}
	
	/**
	 * Floating inputs or Remainder Outputs change the Amount
	 */
	public void resetAmount(MiniNumber zAmount) {
		mAmount = zAmount;
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
		obj.put("remainder", mRemainder);
		
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
		mCoinID   = MiniData.ReadFromStream(zIn);
		mAddress  = MiniData.ReadFromStream(zIn);
		mAmount   = MiniNumber.ReadFromStream(zIn);
		mTokenID  = MiniData.ReadFromStream(zIn);
		
		mFloating  = MiniByte.ReadFromStream(zIn).isTrue();
		mRemainder = MiniByte.ReadFromStream(zIn).isTrue();
	}
	
	public static Coin ReadFromStream(DataInputStream zIn) throws IOException {
		Coin coin = new Coin();
		coin.readDataStream(zIn);
		return coin;
	}
}
