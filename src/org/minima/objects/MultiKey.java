package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRSet;
import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;
import org.minima.utils.digest.Digest;
import org.minima.utils.digest.KeccakDigest;
import org.minima.utils.digest.WinternitzOTSVerify;
import org.minima.utils.digest.WinternitzOTSignature;
import org.minima.utils.json.JSONObject;

/**
 * An implementation of a Tree of Pubkeys.
 * 
 * UNFINISHED..
 * 
 * @author spartacusrex
 *
 */
public class MultiKey implements Streamable {
	
	private static final int WINTERNITZ_NUMBER = 12;
	
	private static int MULTI_TREE_SIZE = 8;
	
	PubPrivKey[] mLeafKeys = new PubPrivKey[MULTI_TREE_SIZE];
	
	/**
	 * Key details
	 */
	MiniData mPrivateSeed;
	MiniData mPublicKey;
	
	int mBitLength;
	
	int mMAX   = 0;
	int mUses  = 0;
	
	private static Digest getHashFunction(int zBitLength) {
		return new KeccakDigest(zBitLength);
	}
	
	public MultiKey(int zBitLength) {
		initKeys(MiniData.getRandomData(zBitLength/8));
	}
	
	public MultiKey(MiniData zPrivateSeed) {
		initKeys(zPrivateSeed);
	}
	
	private void initKeys(MiniData zPrivateSeed) {
		mBitLength = zPrivateSeed.getLength()*8;
		
		//Create a random seed
		mPrivateSeed = zPrivateSeed;

		//Create the Leaf Keys..
		mLeafKeys = new PubPrivKey[MULTI_TREE_SIZE];
		
		//Create a TREE of Public Keys.. 
		for(int i=0;i<MULTI_TREE_SIZE;i++) {
//			mLeafKeys[i] = new PubPrivKey(zPrivateSeed);
			
		}
		
		//Then make an MMRtree of all of them..
		MMRSet mmr = new MMRSet(mBitLength);
		
		//Add each leaf node..
		
		//The root of the tree is the Public Key
		
		//Create a WOTS
		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(mBitLength), WINTERNITZ_NUMBER);
		
		//Get the Public Key..
		mPublicKey  = new MiniData(wots.getPublicKey());
		
		mMAX  = 1;
		mUses = 0;
	}
	
	/**
	 * For reading from stream
	 * @param empty
	 */
	public MultiKey() {}
	
	public MiniData sign(MiniData zData) {
		//Create a WOTS
		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(mBitLength), WINTERNITZ_NUMBER);
		
		//Sign the data..
		byte[] signature = wots.getSignature(zData.getData());
		
		//Return 
		return new MiniData(signature);
	}
	
	public boolean verify(MiniData zData, MiniData zSignature) {
		return verify(mPublicKey, zData, zSignature);
	}
	
	public static boolean verify(MiniData zPubKey, MiniData zData, MiniData zSignature) {
		int bitLength = zPubKey.getLength()*8;
		
		//WOTS Verify
		WinternitzOTSVerify wver = new WinternitzOTSVerify(getHashFunction(bitLength), WINTERNITZ_NUMBER);
		
		//Do it.. get the pubkey..
		byte[] pubkey = wver.Verify(zData.getData(), zSignature.getData());
		
		//Check it
		MiniData resp = new MiniData(pubkey);
		
		//Check..
		return resp.isEqual(zPubKey);
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("bits", mBitLength);
		ret.put("publickey", mPublicKey.to0xString());
		
		return ret;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateSeed() {
		return mPrivateSeed;
	}
	
	public int getBitLength() {
		return mBitLength;
	}
	
	@Override
	public String toString() {
		return mPublicKey.to0xString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mPrivateSeed.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey   = MiniData.ReadFromStream(zIn);
		mPrivateSeed = MiniData.ReadFromStream(zIn);
		mBitLength   = mPrivateSeed.getLength()*8;
	}
}
