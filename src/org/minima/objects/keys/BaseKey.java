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
	protected static final int WINTERNITZ_NUMBER = 12;
	
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
	
	
	public BaseKey() {}
	
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
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("bits", mBitLength);
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
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateSeed() {
		return mPrivateSeed;
	}
	
	public int getBitLength() {
		return mBitLength.getAsInt();
	}
	
	public MiniNumber getMaxUses() {
		return mMaxUses;
	}
	
	public MiniNumber getUses() {
		return mUses;
	}
	
	public void incrementUses() {
		mUses = mUses.increment();
	}
	
	@Override
	public String toString() {
		return mPublicKey.to0xString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mPrivateSeed.writeDataStream(zOut);
		mMaxUses.writeDataStream(zOut);
		mUses.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey   = MiniData.ReadFromStream(zIn);
		mPrivateSeed = MiniData.ReadFromStream(zIn);
		mBitLength   = new MiniNumber(mPrivateSeed.getLength()*8);
		mMaxUses     = MiniNumber.ReadFromStream(zIn);
		mUses        = MiniNumber.ReadFromStream(zIn);
	}
}
