package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.utils.Streamable;

public class PubPrivKey implements Streamable {
	
	MiniData mPrivateSeed;
	
	MiniData mPublicKey;
	
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
		mPublicKey = MiniData.getRandomData(32);
	}
	
	/**
	 * For reading from stream
	 * @param empty
	 */
	public PubPrivKey(boolean empty) {}
	
	public MiniData sign(MiniData32 zData) {
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
	
	public boolean verify(MiniData32 zData, MiniData zSignature) {
		return verify(mPublicKey, zData, zSignature);
	}
	
	public static boolean verify(MiniData zPubKey, MiniData32 zData, MiniData zSignature) {
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
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateSeed() {
		return mPrivateSeed;
	}
	
	@Override
	public String toString() {
		return mPublicKey.toString();
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
	}
}
