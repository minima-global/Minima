package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.objects.mmr.MMRData;
import org.minima.utils.MiniFormat;

public class TestSig {

	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static void main(String[] zArgs) {
		
		/*MiniData data = new MiniData("0x01010102");
		log("Value 1 : "+HORSTUtils.getKeyRef(0, data));
		log("Value 2 : "+HORSTUtils.getKeyRef(1, data));
		*/
		
		log("Start FORS Test");
		
		//Create a message
		MiniData message = new MiniData("0xFFEEDD");
		log("Message : "+message.to0xString());
		
		//Create a Private key seed
		MiniData privkeyseed = new MiniData("0x00112233");
		
		//Hash the message
		log("Generate FORS signing key..");
		FORS fors = new FORS(privkeyseed);
		
		//Get the root public key
		MMRData rootpublickey = fors.getForsRoot();
		
		//Output some data
		log("FORS public key root.. "+rootpublickey.toString());
		
		//Now sign the message
		log("Sign message..");
		FORSSignature sig = fors.signMessage(message);
		
		/*log("Signature : ");
		System.out.println();
		log(MiniFormat.JSONPretty(sig.toJSON()));
		*/
		
		//Now verify the message
		log("Verify : "+fors.verifySignature(message, sig, rootpublickey));
		
		
		/*
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
		
		log("Signature : ");
		System.out.println();
		//log(MiniFormat.JSONPretty(sig.toJSON()));
		
		//Now verify the message
		boolean valid = horst.verifySignature(message, sig, rootpublickey);
		log("Verify : "+valid);
		*/
	}
	
}
