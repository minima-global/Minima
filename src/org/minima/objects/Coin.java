package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Coin implements Streamable {
	
	/**
	 * Outputs don't specify a coinid
	 */
	public static final MiniData32 MINIMA_TOKENID = new MiniData32("0x00");
	
	public static final MiniData32 COINID_OUTPUT  = new MiniData32("0x00");
	
	public static final MiniData32 TOKENID_CREATE = new MiniData32("0xFF");
	
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
	MiniData32 	mCoinID;
	
	/**
	 * The RamAddress.
	 *  
	 * This is the hash of the RamScript that controls this coin. This is ALWAYS present
	 */
	MiniData32 	mAddress;
	
	/**
	 * The Value of this Coin. This is ALWAYS present
	 */
	MiniNumber 	mAmount;
	
	/**
	 * Tokens are Native in Minima. All inputs and outputs have them. MINIMA the default is 0x00
	 */
	MiniData32  mTokenID;
	
//	/**
//	 * The TokenID is proved by the hash of the coinid (MiniData32) | total minima used (MiniNumber) | and total digits (MinByte).
//	 * If TokenID = 0x00 than this is blank..
//	 */
//	MiniData 	mTokenProof;
	
	/**
	 * Main Constructor
	 */
	public Coin(MiniData32 zCoinID, MiniData32 zAddress, MiniNumber zAmount, MiniData32 zTokenID) {
		mCoinID  = zCoinID;
		mAddress = zAddress;
		mAmount  = zAmount;
		mTokenID = zTokenID;
	}
	
	/**
	 * Required For Streamable.
	 */
	private Coin() {}
		
	public MiniData32 getCoinID() {
		return mCoinID;
	}
	
	public MiniData32 getAddress() {
		return mAddress;
	}
	
	public MiniNumber getAmount() {
		return mAmount;
	}

	public MiniData32 getTokenID() {
		return mTokenID;
	}
	
	/**
	 * When creating a token a token of LESS THANN 255 tells how many decimal places to use..
	 */
	public static MiniData32 getTokenCreationID(int zDecimalPlaces) {
		int totplaces = zDecimalPlaces;
		if(totplaces > 255) {
			totplaces = 255;
		}
		
		//create the number
		MiniByte tot = new MiniByte(totplaces);
		
		//Now generate..
		byte[] data = new byte[1];
		data[0] = tot.getByteValue();
		
		return new MiniData32(data);
	}
	
	@Override
	public String toString() {
		return  toJSON().toString();
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("coinid", mCoinID.toString());
		obj.put("address", mAddress.toString());
		obj.put("amount", mAmount.toString());
		obj.put("tokenid", mTokenID.toString());
		
		return obj;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCoinID.writeDataStream(zOut);
		mAddress.writeDataStream(zOut);
		mAmount.writeDataStream(zOut);
		mTokenID.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID  = MiniData32.ReadFromStream(zIn);
		mAddress = MiniData32.ReadFromStream(zIn);
		mAmount  = MiniNumber.ReadFromStream(zIn);
		mTokenID = MiniData32.ReadFromStream(zIn);
	}	
	
	public static Coin ReadFromStream(DataInputStream zIn) throws IOException {
		Coin coin = new Coin();
		coin.readDataStream(zIn);
		return coin;
	}
}
