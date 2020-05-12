package org.minima.objects.keys;

import org.minima.database.mmr.MMRSet;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;

public class MultiKey extends BaseKey {
	
	int MULTI_KEYS = 8;
	
	boolean mSingle;
	
	BaseKey[] mBaseKeys;
	
	MMRSet mMMR;
	
//	public MultiKey(int zBitLength) {
//		initKeys(MiniData.getRandomData(zBitLength/8));
//	}
	
	public MultiKey(MiniData zPrivateSeed, int zKeys, boolean zSingle) {
		super();
		MULTI_KEYS = zKeys;
		mSingle    = zSingle;
		
		initKeys(zPrivateSeed);
	}
	
	private void initKeys(MiniData zPrivateSeed) {
		mBitLength = new MiniNumber(zPrivateSeed.getLength()*8);
		
		mPrivateSeed = zPrivateSeed;

		mMaxUses  = new MiniNumber(MULTI_KEYS);
		mUses     = MiniNumber.ZERO;
		
		//Create the Key Tree
		mBaseKeys = new SingleKey[MULTI_KEYS];
		
		//Now create the MMR tree
		mMMR = new MMRSet(mBitLength.getAsInt());
				
		//Create all the keys..
		for(int i=0;i<MULTI_KEYS;i++) {
			//Create the Key
			if(mSingle) {
				mBaseKeys[i] = new SingleKey(mBitLength.getAsInt());	
			}else {
				MiniData privkey  = new MiniData("0x"+Integer.toHexString(i));
				MiniData fullpriv = mPrivateSeed.concat(privkey);
				MiniData cascpriv = new MiniData( Crypto.getInstance().hashData(fullpriv.getData()) );
				mBaseKeys[i]      = new MultiKey(cascpriv, MULTI_KEYS, true);
			}
			
			//Add to the tree
			mMMR.addLeafNode(mBaseKeys[i].getPublicKey());
		}
		
		//Finalise the set
		mMMR.finalizeSet();
		
		//Get the root of the tree..
		mPublicKey = mMMR.getMMRRoot().getFinalHash();
	}
	
	/**
	 * For reading from stream
	 * @param empty
	 */
	public MultiKey() {}

	@Override
	public MiniData sign(MiniData zData) {
		//Which key are we on..
		int key = getUses().getAsInt();
		
		System.out.println("KEY :"+key);
		
		//Get that Key..
		MiniData pubkey    = mBaseKeys[key].getPublicKey();
		MiniData signature = mBaseKeys[key].sign(zData);
		MiniData proof     = mMMR.getFullProofToRoot(new MiniInteger(key)).getChainSHAProof();
		
		//Once used you cannot use it again..
		incrementUses();
		
		//Create a multi sig..
		MultiSig sig = new MultiSig(pubkey, proof, signature);
		
		return sig.getCompleteSig();
	}

	@Override
	public boolean verify(MiniData zData, MiniData zSignature) {
		MultiSig sig = new MultiSig(zSignature);
		
		//First check the Public Key is correct
		if(!sig.getRootKey().isEqual(mPublicKey)) {
			return false;
		}
		
		//Create a Single Key
		SingleKey skey = new SingleKey();
		skey.setPublicKey(sig.getPublicKey());
		
		//Now check the Signature
		boolean ver = skey.verify(zData, sig.getSignature());
		
		return ver;
	}
	
	
	public static void main(String[] zargs) {		
		
//		//get some data
//		MiniData privseed = MiniData.getRandomData(20);
//				
//		//Create a new key
//		MultiKey mkey = new MultiKey(privseed,true);
//		
//		//get some data
//		MiniData data = MiniData.getRandomData(32);
//		
//		//Sign it..
//		MiniData sig = mkey.sign(data);
//		
//		System.out.println("Data    : "+data);
//		System.out.println("SigLen  : "+sig.getLength());
//		System.out.println("Sig     : "+sig.to0xString());
//		
//		//Verify it..
//		boolean ver = mkey.verify(data, sig);
//		
//		System.out.println("Verify  : "+ver);
//		
//		//Sign it..
//		System.out.println();
//		sig = mkey.sign(data);
//		
//		System.out.println("SigLen  : "+sig.getLength());
//		System.out.println("Sig     : "+sig.to0xString());
//		
//		//Verify it..
//		ver = mkey.verify(data, sig);
//		
//		System.out.println("Verify  : "+ver);
//		
		
	}

	
}
