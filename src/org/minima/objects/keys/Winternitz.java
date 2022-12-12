package org.minima.objects.keys;

import org.bouncycastle.crypto.digests.SHA3Digest;
import org.bouncycastle.pqc.crypto.gmss.util.WinternitzOTSVerify;
import org.bouncycastle.pqc.crypto.gmss.util.WinternitzOTSignature;
import org.minima.objects.base.MiniData;

public class Winternitz {

	/**
	 * The WINTERNITZ hash value.. 8 is fast
	 */
	private static final int WINTERNITZ_VALUE = 8;
	
	/**
	 * The Winternitz key.. only create it once..
	 */
	private WinternitzOTSignature mWOTS = null;
	
	MiniData mPrivateSeed;
	
	MiniData mPublicKey;
	
	public Winternitz() {}
	
	public Winternitz(MiniData zPrivateSeed) {
		//Create a random seed
		mPrivateSeed = zPrivateSeed;
		
		mWOTS = new WinternitzOTSignature(mPrivateSeed.getBytes(), new SHA3Digest(256), WINTERNITZ_VALUE);
		
		//Get the Public Key..
		mPublicKey = new MiniData(mWOTS.getPublicKey());
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData sign(MiniData zData) {
		//Sign the data..
		byte[] signature = mWOTS.getSignature(zData.getBytes());
		
		//Return 
		return new MiniData(signature);
	}

	public static boolean verify(MiniData zPublicKey, MiniData zData, MiniData zSignature) {
		//WOTS Verify
		WinternitzOTSVerify wver = new WinternitzOTSVerify(new SHA3Digest(256), WINTERNITZ_VALUE);
		
		//Do it.. get the pubkey..
		byte[] pubkey = wver.Verify(zData.getBytes(), zSignature.getBytes());
		
		//Check it
		MiniData resp = new MiniData(pubkey);
		
		//Check..
		return resp.isEqual(zPublicKey);
	}
}
