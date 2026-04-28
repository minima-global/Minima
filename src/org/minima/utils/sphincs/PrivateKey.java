package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;

public class PrivateKey {

	/*
	 * The Private Key is an array of random 32 byte hashes
	 */
	MiniData[] mPrivateKeys;
	
	public PrivateKey(MiniData zSeed, int zKeySizeBitParam) {
		
		//How many keys in total
		int totalkeys = (int)Math.pow(2, zKeySizeBitParam);
		
		//Generate all the keys
		mPrivateKeys = new MiniData[totalkeys];
		for(int i=0;i<totalkeys;i++) {
			mPrivateKeys[i] = Crypto.getInstance().hashAllObjects(zSeed, new MiniNumber(i));
		}
	}
	
	public int getSize() {
		return mPrivateKeys.length;
	}
	
	public MiniData getKey(int zPos) {
		return mPrivateKeys[zPos];
	}
}
