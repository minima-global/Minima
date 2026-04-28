package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;

public class TestSig {

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
		
		//Now sign the message
		log("Sign message..");
		Signature sig = horst.signMessage(message);
		
		//Now verify the message
		boolean valid = horst.verifySignature(message, sig);
		log("Verify : "+valid);
	}
	
}
