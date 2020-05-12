package org.minima.objects.keys;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.digest.WinternitzOTSVerify;
import org.minima.utils.digest.WinternitzOTSignature;

public class SingleKey extends BaseKey {

	public SingleKey() {
		super();
	}
	
	public SingleKey(int zBitLength) {
		super();
		initKeys(MiniData.getRandomData(zBitLength/8));
	}
	
	public SingleKey(MiniData zPrivateSeed) {
		super();
		initKeys(zPrivateSeed);
	}
	
	private void initKeys(MiniData zPrivateSeed) {
		//Number of Bits of security
		mBitLength = new MiniNumber(zPrivateSeed.getLength()*8);
		
		//Create a random seed
		mPrivateSeed = zPrivateSeed;

		//Create a WOTS
		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(mBitLength), WINTERNITZ_NUMBER);
		
		//Get the Public Key..
		mPublicKey  = new MiniData(wots.getPublicKey());
	}
	
	/**
	 * Only use this to VERIFY - cannot sign
	 * @param zPublicKey
	 */
	public void setPublicKey(MiniData zPublicKey) {
		mPublicKey = zPublicKey;
		mBitLength = new MiniNumber(zPublicKey.getLength()*8); 
	}
	
	@Override
	public MiniData sign(MiniData zData) {
		//Create a WOTS
		WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(mBitLength), WINTERNITZ_NUMBER);
		
		//Sign the data..
		byte[] signature = wots.getSignature(zData.getData());
		
		//Return 
		return new MiniData(signature);
	}

	@Override
	public boolean verify(MiniData zData, MiniData zSignature) {
		//WOTS Verify
		WinternitzOTSVerify wver = new WinternitzOTSVerify(getHashFunction(mBitLength), WINTERNITZ_NUMBER);
		
		//Do it.. get the pubkey..
		byte[] pubkey = wver.Verify(zData.getData(), zSignature.getData());
		
		//Check it
		MiniData resp = new MiniData(pubkey);
		
		//Check..
		return resp.isEqual(mPublicKey);
	}
}
