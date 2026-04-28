package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.utils.Crypto;

public class HORST {

	public static final int NUMBER_OF_CHUNK_BYTES = 16;
	
	PrivateKey mPrivateKey;
	PublicKey mPublicKey;
	
	public HORST(MiniData zSeed) {
		
		//Create a Private Key
		mPrivateKey = new PrivateKey(zSeed, 8);
		
		//Create a Public Key
		mPublicKey = new PublicKey(mPrivateKey);
	}
	
	public Signature signMessage(MiniData zMessage) {
		
		//Hash the message
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Shrink the message
		MiniData shrinkedmessage = HORSTUtils.shrinkData(NUMBER_OF_CHUNK_BYTES, hm);
		
		Signature sig = new Signature();
		
		//Set the root hash of the public key tree
		sig.setPublicKeyTreeRoot(mPublicKey.getPublicKeyTreeRoot());
		
		for(int i=0;i<NUMBER_OF_CHUNK_BYTES;i++) {
			
			//Get the value ref of the message
			int ref = HORSTUtils.getKeyRef(i, shrinkedmessage);
			
			//Add the simple Signature / Private key value (pre-image of public key)
			sig.getSignatureValues().add(mPrivateKey.getKey(ref));
			
			//Add the Public key proof
			sig.getPublicKeyTreeProofs().add(mPublicKey.getKeyTreeProof(ref));
		}
		
		return sig;
	}
	
	public static boolean verifySignature(MiniData zMessage, Signature zSignature) {
		return false;
	}
}
