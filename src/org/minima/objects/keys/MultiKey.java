package org.minima.objects.keys;

import org.minima.database.mmr.MMRSet;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;

public class MultiKey extends BaseKey {
	
	int MULTI_KEYS = 8;
	
	//How many levels of Key Trees..
	int mLevels;
	
	//The Leaf Node Keys..
	SingleKey[] mBaseKeys;
	
	//The Current Leaf Node being used..
	int mLeaf = -1;
	
	//The Current MultiKey for the leafnode..
	MultiKey mCurrentBase = null;
	
	//The signature of the Root of the current base
	MiniData mCurrentSignature = null;
	
	MMRSet mMMR;
	
	public MultiKey(MiniData zPrivateSeed, int zKeys, int zLevels) {
		super();
		MULTI_KEYS = zKeys;
		mLevels    = zLevels;
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
			mBaseKeys[i] = new SingleKey(mBitLength.getAsInt());	
			
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
		int keynum = getUses().getAsInt();
		
		//Once used you cannot use it again..
		incrementUses();
		
		//How many signatures per leaf..
		int perleaf = (int) Math.pow(MULTI_KEYS, mLevels-1);
		System.out.println("LEVEL:"+mLevels+" keys per leaf:"+perleaf);
		
		//Which leaf node are we using..
		int leaf = (int)(keynum / perleaf);
		System.out.println("LEVEL "+mLevels+" Keynum : "+keynum+" leaf:"+leaf);
		
		if(mLevels == 1) {
			if(keynum>=getMaxUses().getAsInt()) {
				System.out.println("ERROR Key used too many times! MAX:"+getMaxUses());
				//Create an INVALID multi sig..
				MiniData zero = new MiniData("0x0000");
				MultiSig sig  = new MultiSig(zero,zero, zero);
				return sig.getCompleteSig();
			}
			
			//Get that Key..
			MiniData pubkey    = mBaseKeys[keynum].getPublicKey();
			MiniData signature = mBaseKeys[keynum].sign(zData);
			MiniData proof     = mMMR.getFullProofToRoot(new MiniInteger(keynum)).getChainSHAProof();
			
			//Create a multi sig..
			MultiSig sig = new MultiSig(pubkey, proof, signature);
			
			return sig.getCompleteSig();
			
		}
		
		//Are we on the same leaf or a new one..
		if(leaf != mLeaf) {
			//Store
			mLeaf = leaf;
			
			//Get that key..
			SingleKey leafkey = mBaseKeys[leaf];
		
			//Create a private seed based of this see..
			MiniData leafnumdata = new MiniData(BaseConverter.numberToHex(leaf));
			System.out.println("LEVEL "+mLevels+" leaf "+leaf+" "+leafnumdata.to0xString());
			
			//Now create the private seed..
			MiniData leafpriv = new MiniData( Crypto.getInstance().hashData(mPrivateSeed.concat(leafnumdata).getData()) );
			
			//Create a new Multi Key at this leaf position..
			mCurrentBase = new MultiKey(leafpriv, MULTI_KEYS, mLevels-1); 
			
			//Get the Base..
			MiniData rootkey = mCurrentBase.getPublicKey();
			
			//Sign that..
			mCurrentSignature = leafkey.sign(rootkey);
		}	
		
		//Use the current base 
		MiniData signature = mCurrentBase.sign(zData);
		
		//Create a multi sig..
		MultiSig sig = new MultiSig(mCurrentSignature, signature);
		
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
		
		//get some data
		MiniData privseed = MiniData.getRandomData(20);
				
		//Create a new key
		MultiKey mkey = new MultiKey(privseed, 4, 1);
		
		//get some data
		MiniData data = MiniData.getRandomData(20);
		System.out.println("Data    : "+data);
		System.out.println();
		
		//Sign it..
		MiniData sig = mkey.sign(data);
		System.out.println("SigLen  : "+sig.getLength());
		System.out.println("SigHash : "+Crypto.getInstance().hashObject(sig,160).to0xString());
		System.out.println("Verify  : "+mkey.verify(data, sig));
		System.out.println();
		
		//Sign it..
		sig = mkey.sign(data);
		System.out.println("SigLen  : "+sig.getLength());
		System.out.println("SigHash : "+Crypto.getInstance().hashObject(sig,160).to0xString());
		System.out.println("Verify  : "+mkey.verify(data, sig));
		System.out.println();
		
		//Sign it..
		sig = mkey.sign(data);
		System.out.println("SigLen  : "+sig.getLength());
		System.out.println("SigHash : "+Crypto.getInstance().hashObject(sig,160).to0xString());
		System.out.println("Verify  : "+mkey.verify(data, sig));
		System.out.println();
		
		
	}

	
}
