package org.minima.objects.keys;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.digest.Digest;
import org.minima.utils.digest.KeccakDigest;
import org.minima.utils.json.JSONObject;

public abstract class BaseKey implements Streamable {

	/**
	 * The Winternitz number used by all the Lamport Signatures
	 */
	private MiniNumber mWinternitz = MiniNumber.TWELVE;
	
	/**
	 * Security of the signature in Bits
	 */
	protected MiniNumber mBitLength;
	
	/**
	 * Private Key - form which everything can be made
	 */
	protected MiniData mPrivateSeed;
	
	/**
	 * Public Key
	 */
	protected MiniData mPublicKey;

	/**
	 * Number of Times you can use this Key
	 */
	protected MiniNumber mMaxUses;
	
	/**
	 * Number of Times you have used this key
	 */
	protected MiniNumber mUses;
	
	/**
	 * What Level in the tree key
	 */
	protected MiniNumber mLevel;
	
	
	public BaseKey() {}
	
	/**
	 * Initialise variables with the private key
	 * @param zPrivateSeed
	 */
	protected abstract void initKeys(MiniData zPrivateSeed);
		
	/**
	 * Sign arbitrary data
	 * 
	 * @param zData
	 * @return
	 */
	public abstract MiniData sign(MiniData zData);
	
	/**
	 * Verify this data with this signature
	 * @param zData
	 * @param zSignature
	 * @return
	 */
	public abstract boolean verify(MiniData zData, MiniData zSignature);
		
	/**
	 * The Digest used by the Signing algorithm
	 * 
	 * @param zBitLength
	 * @return
	 */
	protected static Digest getHashFunction(MiniNumber zBitLength) {
		return new KeccakDigest(zBitLength.getAsInt());
	}

	/**
	 * The total amount of times you can use this key 
	 * @return
	 */
	public MiniNumber getTotalAllowedUses() {
		return mMaxUses.pow(mLevel.getAsInt());
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("bits", mBitLength);
		ret.put("uses", getUses().toString());
		ret.put("allowed", getTotalAllowedUses().toString());
		ret.put("publickey", mPublicKey.to0xString());
		
		return ret;
	}
	
	/**
	 * Only use this to VERIFY - cannot sign
	 * @param zPublicKey
	 */
	public void setPublicKey(MiniData zPublicKey) {
		mPublicKey = zPublicKey;
		mBitLength = new MiniNumber(zPublicKey.getLength()*8); 
	}
	
	public MiniNumber getLevel() {
		return mLevel;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateSeed() {
		return mPrivateSeed;
	}
	
	public int getBitLength() {
		return mBitLength.getAsInt();
	}
	
	public int getWinternitz() {
		return mWinternitz.getAsInt();
	}
	
	public MiniNumber getMaxUses() {
		return mMaxUses;
	}
	
	public MiniNumber getUses() {
		return new MiniNumber(mUses.getAsBigInteger());
	}
	
	public void incrementUses() {
		mUses = mUses.increment();
	}
	
	public void setUses(MiniNumber zUses) {
		mUses = zUses;
	}
	
	@Override
	public String toString() {
		return mPublicKey.to0xString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mPrivateSeed.writeDataStream(zOut);
		mWinternitz.writeDataStream(zOut);
		mLevel.writeDataStream(zOut);
		mMaxUses.writeDataStream(zOut);
		mUses.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey   = MiniData.ReadFromStream(zIn);
		mPrivateSeed = MiniData.ReadFromStream(zIn);
		
		//FOR NOW MUST BE 12..in future.. maybe higher..
		mWinternitz  = MiniNumber.ReadFromStream(zIn);
		if(!mWinternitz.isEqual(MiniNumber.TWELVE)) {
			throw new IOException("INVALID Winternitz : must be 12!");
		}
		
		//Important values
		mLevel       = MiniNumber.ReadFromStream(zIn);
		mMaxUses     = MiniNumber.ReadFromStream(zIn);
		mUses        = MiniNumber.ReadFromStream(zIn);
		
		//Init the variables 
		initKeys(mPrivateSeed);
	}
}
