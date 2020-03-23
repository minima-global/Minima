package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;

public class PubPrivKey implements Streamable {
	
	MiniData mPrivateSeed;
	
	MiniHash mPublicKey;
	
	//HACK NO BOUNCY
//	private static final int WinternitzNumber = 8;
//	private static Digest getHashFunction() {
//		return new KeccakDigest(256);
//	}
	
	public PubPrivKey() {
		this(MiniData.getRandomData(32));
	}
	
	public PubPrivKey(MiniData zPrivateSeed) {
		//Create a random seed
		mPrivateSeed = zPrivateSeed;

//		//Create a WOTS
//		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(), WinternitzNumber);
//		
//		//Get the Public Key..
//		mPublicKey  = new MiniData(wots.getPublicKey());
		
		//HACK NO BOUNCY
		mPublicKey = Crypto.getInstance().hashObject(zPrivateSeed);
	}
	
	/**
	 * For reading from stream
	 * @param empty
	 */
	public PubPrivKey(boolean empty) {}
	
	public MiniData sign(MiniHash zData) {
//		//Create a WOTS
//		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(), WinternitzNumber);
//		
//		//Sign the data..
//		byte[] signature = wots.getSignature(zData.getData());
//		
//		//Return 
//		return new MiniData(signature);
		
		//HACK NO BOUNCY
		return MiniData.getRandomData(64);
	}
	
	public boolean verify(MiniHash zData, MiniData zSignature) {
		return verify(mPublicKey, zData, zSignature);
	}
	
	public static boolean verify(MiniHash zPubKey, MiniHash zData, MiniData zSignature) {
//		//WOTS Verify
//		WinternitzOTSVerify wver = new WinternitzOTSVerify(getHashFunction(), WinternitzNumber);
//		
//		//Do it.. get the pubkey..
//		byte[] pubkey = wver.Verify(zData.getData(), zSignature.getData());
//		
//		//Check it
//		MiniData resp = new MiniData(pubkey);
//		
//		//Check..
//		return resp.isExactlyEqual(zPubKey);
		
		//HACK NO BOUNCY
		return true;
	}
	
	public MiniHash getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateSeed() {
		return mPrivateSeed;
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
		mPublicKey   = MiniHash.ReadFromStream(zIn);
		mPrivateSeed = MiniData.ReadFromStream(zIn);
	}
}
