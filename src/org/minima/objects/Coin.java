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
	
//	/**
//	 * The TokenID is proved by the hash of the coinid (MiniData32) | total minima used (MiniNumber) | and total digits (MinByte).
//	 * If TokenID = 0x00 than this is blank..
//	 */
//	MiniData 	mTokenProof;
	
	/**
	 * Main Constructor
	 */
	public Coin(MiniHash zCoinID, MiniHash zAddress, MiniNumber zAmount, MiniHash zTokenID) {
		mCoinID  = zCoinID;
		mAddress = zAddress;
		mAmount  = zAmount;
		mTokenID = zTokenID;
	}
	
	/**
	 * Required For Streamable.
	 */
	private Coin() {}
		
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
	
	/**
	 * When creating a token a token of LESS THANN 255 tells how many decimal places to use..
	 */
	public static MiniHash getTokenCreationID(int zDecimalPlaces) {
		int totplaces = zDecimalPlaces;
		if(totplaces > 255) {
			totplaces = 255;
		}
		
		//create the number
		MiniByte tot = new MiniByte(totplaces);
		
		//Now generate..
		byte[] data = new byte[1];
		data[0] = tot.getByteValue();
		
		return new MiniHash(data);
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
		mCoinID  = MiniHash.ReadFromStream(zIn);
		mAddress = MiniHash.ReadFromStream(zIn);
		mAmount  = MiniNumber.ReadFromStream(zIn);
		mTokenID = MiniHash.ReadFromStream(zIn);
	}	
	
	public static Coin ReadFromStream(DataInputStream zIn) throws IOException {
		Coin coin = new Coin();
		coin.readDataStream(zIn);
		return coin;
	}
}
