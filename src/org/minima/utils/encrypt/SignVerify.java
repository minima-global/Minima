package org.minima.utils.encrypt;

import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.Signature;

import org.minima.objects.base.MiniData;

public class SignVerify {

	public static final String SIGN_ALGO 		= "SHA256withRSA";
	
	public static byte[] sign(byte[] zPrivateKey, byte[] zMessage) throws Exception {
		
		PrivateKey privateKey 	= GenerateKey.convertBytesToPrivate(zPrivateKey);
		
		Signature signature 	= Signature.getInstance(SIGN_ALGO);
        signature.initSign(privateKey, new SecureRandom());
        signature.update(zMessage);
        		
		return signature.sign();
	}
	
	public static boolean verify(byte[] zPublicKey, byte[] zMessage, byte[] zSignature) throws Exception {
		
		PublicKey publicKey		= GenerateKey.convertBytesToPublic(zPublicKey);
		
		Signature signature 	= Signature.getInstance(SIGN_ALGO);
        signature.initVerify(publicKey);
        signature.update(zMessage);
        
		return signature.verify(zSignature);
	}
	
	public static void main(String[] zArgs) throws Exception {
		
		//We use Bouncy
		Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
        
		//Get a Key Pair..
		KeyPair kp = GenerateKey.generateKeyPair();
		
		//Get the byte data..
		byte[] pubk 	= kp.getPublic().getEncoded();
		byte[] privk 	= kp.getPrivate().getEncoded();
		
		MiniData msg 	= MiniData.getRandomData(1024);
        byte[] message 	= msg.getBytes();
        
        //The signature
		byte[] sigBytes = sign(privk, message);

        System.out.println(sigBytes.length);
		
        boolean result = verify(pubk, message, sigBytes);
		
        System.out.println(result);
	}
}
