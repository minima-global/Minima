package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMRData;
import org.minima.utils.Crypto;

public class HORST {

	public static final int NUMBER_OF_CHUNK_BYTES = 16;
	
	PrivateKey mPrivateKey;
	PublicKey mPublicKey;
	
	public HORST(MiniData zSeed) {
		
		//Create a Private Key
		mPrivateKey = new PrivateKey(zSeed);
		
		//Create a Public Key
		mPublicKey = new PublicKey(mPrivateKey);
	}
	
	public PublicKey getPublicKey() {
		return mPublicKey;
	}
	
	public PrivateKey getPrivateKey() {
		return mPrivateKey;
	}
	
	public Signature signMessage(MiniData zMessage) {
		
		//Hash the message - 32 bytes
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Start a new signature
		Signature sig = new Signature();
		
		//Calculate all the signature values
		for(int i=0;i<NUMBER_OF_CHUNK_BYTES;i++) {
			
			//Get the value reference of the message
			int ref = getKeyRef(i, hm);
			
			//Add the simple Signature / Private key value (pre-image of public key)
			sig.getSignatureValues().add(mPrivateKey.getKey(ref));
			
			//Add the Public key proof
			sig.getPublicKeyTreeProofs().add(mPublicKey.getKeyTreeProof(ref));
		}
		
		return sig;
	}
	
	public static boolean verifySignature(MiniData zMessage, Signature zSignature, MMRData zPublicKeyRoot) {
		
		//Hash the message
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Now check each value of the signature
		for(int i=0;i<NUMBER_OF_CHUNK_BYTES;i++) {
			
			//Get the value reference of the message
			int ref = getKeyRef(i, hm);
			
			//Get the signature / private key value
			MiniData privkey = zSignature.getSignatureValues().get(i);
			
			//Hash that to get the public key value
			MiniData checkpreimage = new MiniData(Crypto.getInstance().hashData(privkey.getBytes()));
			
			//Now check this is in the public key tree at the correct position
			MMRData leaf 		= MMRData.CreateMMRDataLeafNode(checkpreimage, new MiniNumber(ref));
			MMRData checkroot 	= zSignature.getPublicKeyTreeProofs().get(i).calculateProof(leaf);
			if(!checkroot.isEqual(zPublicKeyRoot)) {
				return false;
			}
		}
		
		return true;
	}
	
	public static int getKeyRef(int zPos, MiniData zOrig) {
		byte[] allbytes	= zOrig.getBytes();
		
		int pos = zPos*2;
		
		byte[] twobyte = new byte[2];
		twobyte[0]	   = allbytes[pos];
		twobyte[1]	   = allbytes[pos+1];
		
		MiniData chunk = new MiniData(twobyte);
		
		return chunk.getDataValue().intValueExact();
	}
}
