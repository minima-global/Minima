package org.minima.utils.sphincs.HORST;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMR;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMREntryNumber;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.Crypto;

public class PublicKey {

	/**
	 * The public keys are the hash of the private keys
	 * 
	 * The private keys are the unknowable pre-image of the public key
	 */
	MiniData[] mPublicKeys;
	
	/**
	 * The keys are also stored in a Tree
	 */
	MMR mPublicKeyTree;
	
	/**
	 * The root of the tree
	 */
	MMRData mPublicKeyTreeRoot;
	
	public PublicKey(PrivateKey zPrivKey) {
		
		//How many keys in total
		int totalkeys = zPrivKey.getSize();
		
		//Create an MMR
		mPublicKeyTree = new MMR();
		
		//Generate the public keys
		mPublicKeys = new MiniData[totalkeys];
		for(int i=0;i<totalkeys;i++) {
			
			//Create a public key as the hash of the private key
			byte[] privkey = zPrivKey.getKey(i).getBytes();
			mPublicKeys[i] = new MiniData(Crypto.getInstance().hashData(privkey));
			
			//Add this value to our Public Key Tree - AND add the order info to the data using the SUM value :)
			MMRData leaf = MMRData.CreateMMRDataLeafNode(mPublicKeys[i], new MiniNumber(i));
			mPublicKeyTree.addEntry(leaf);
		}
		
		//Finalize the tree..
		mPublicKeyTree.finalizeSet();
		
		//Get the root..
		mPublicKeyTreeRoot = mPublicKeyTree.getRoot();
	}
	
	public int getSize() {
		return mPublicKeys.length;
	}
	
	public MiniData getKey(int zPos) {
		return mPublicKeys[zPos];
	}
	
	public MMRData getPublicKeyTreeRoot() {
		return mPublicKeyTreeRoot;
	}
	
	public MMRProof getKeyTreeProof(int zPos) {
		return mPublicKeyTree.getProof(new MMREntryNumber(zPos));
	}
}
