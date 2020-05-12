package org.minima.objects.keys;

import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRSet;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;

public class MultiKey extends BaseKey {
	
	int MULTI_KEYS = 8;
	
	BaseKey[] mBaseKeys;
	
	public MultiKey(int zBitLength) {
		initKeys(MiniData.getRandomData(zBitLength/8));
	}
	
	public MultiKey(MiniData zPrivateSeed) {
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
		MMRSet mmr = new MMRSet(mBitLength.getAsInt());
				
		//Create all the keys..
		for(int i=0;i<MULTI_KEYS;i++) {
			//Create the Key
			mBaseKeys[i] = new SingleKey(mBitLength.getAsInt());
		
			//Add to the tree
			MMREntry leaf = mmr.addLeaNode(mBaseKeys[i].getPublicKey());
		}
		
		//Finalise the set
		mmr.finalizeSet();
		
		//Get the root of the tree..
		mPublicKey = mmr.getMMRRoot().getFinalHash();
	}
	
	/**
	 * For reading from stream
	 * @param empty
	 */
	public MultiKey() {}

	@Override
	public MiniData sign(MiniData zData) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean verify(MiniData zData, MiniData zSignature) {
		// TODO Auto-generated method stub
		return false;
	}
	
	
	public static void main(String[] zargs) {		
		int bitlength = 160;
		int keynum    = 4;
		
		//Create a tree of keys..
		PubPrivKey[] keys = new PubPrivKey[keynum];
		
		//Now create the MMR tree
		MMRSet mmr = new MMRSet(bitlength);
				
		//Create all the keys..
		for(int i=0;i<keynum;i++) {
			//Create the Key
			keys[i] = new PubPrivKey(bitlength);
		
			//Add to the tree
			MMREntry leaf = mmr.addLeaNode(keys[i].getPublicKey());
		}
		
		//Finalize the set
		mmr.finalizeSet();
		
		//Get the root..
		MiniData root = mmr.getMMRRoot().getFinalHash();
		System.out.println("MULTI PUB KEY : "+root.to0xString());
		
		MiniData rand = MiniData.getRandomData(32);
		System.out.println("DATA to SIGN : "+rand.to0xString());
		
		//Sign it..
		System.out.println();
		for(int i=0;i<1;i++) {
			MiniData pubkey    = keys[i].getPublicKey();
			MiniData signature = keys[i].sign(rand);
			MiniData proof     = mmr.getFullProofToRoot(new MiniInteger(i)).getChainSHAProof();
			
			//Create a multi sig..
			MultiSig sig = new MultiSig(pubkey, proof, signature);
			
			System.out.println("MultiSig Key "+i+" : "+sig.getCompleteSig().getLength());	
		}
			
	}

	
}
