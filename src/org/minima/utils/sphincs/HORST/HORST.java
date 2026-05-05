package org.minima.utils.sphincs.HORST;

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
	
	public MMRData getHORSTRoot() {
		return mPublicKey.getPublicKeyTreeRoot();
	}
	
	public PublicKey getPublicKey() {
		return mPublicKey;
	}
	
	public PrivateKey getPrivateKey() {
		return mPrivateKey;
	}
	
	public HORSTSignature signMessage(MiniData zMessage) {
		
		//Hash the message - 32 bytes
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Start a new signature
		HORSTSignature sig = new HORSTSignature();
		
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
	
	public static boolean verifySignature(MiniData zMessage, HORSTSignature zSignature, MMRData zPublicKeyRoot) {
		
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
	
	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static void main(String[] zArgs) {
		
		
		log("Start HORST Test");
		
		//Create a message
		MiniData message = new MiniData("0xFFEEDD");
		log("Message : "+message.to0xString());
		
		//Create a Private key seed
		MiniData privkeyseed = new MiniData("0x00112233");
		
		//Hash the message
		log("Generate HORST signing key..");
		HORST horst = new HORST(privkeyseed);
		
		//Get the root public key
		MMRData rootpublickey = horst.getPublicKey().getPublicKeyTreeRoot();
		
		//Output some data
		log("HORST private key size.. "+horst.getPrivateKey().getSize());
		log("HORST public key root.. "+rootpublickey.toString());
		
		//Now sign the message
		log("Sign message..");
		HORSTSignature sig = horst.signMessage(message);
		
		//Write to stream
		MiniData stream = MiniData.getMiniDataVersion(sig); 
		
		//Read from stream
		HORSTSignature readsig = HORSTSignature.convertMiniDataVersion(stream);
		log("Signature Size : "+stream.getLength());
		
		log("Signature : ");
		System.out.println();
		//log(MiniFormat.JSONPretty(sig.toJSON()));
		
		//Now verify the message
		boolean valid = horst.verifySignature(message, readsig, rootpublickey);
		log("Verify : "+valid);
		
	}
}
