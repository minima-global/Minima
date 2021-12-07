package org.minima.utils.encrypt;

import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class GenerateKey {

	public static final String 	ASYMETRIC_ALGORITHM 	= "RSA";
	private static final String SYMETRIC_ALGORITHM  	= "AES";
	
	public static final String 	CRYPTO_PROVIDER 		= "BC";
	
	public static KeyPair generateKeyPair() throws Exception {

		SecureRandom random 	= SecureRandom.getInstanceStrong();
		
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance(ASYMETRIC_ALGORITHM,CRYPTO_PROVIDER);
        keyGen.initialize(1024, random);
        
        KeyPair generateKeyPair = keyGen.generateKeyPair();
        
        return generateKeyPair;
    }
	
	public static PublicKey convertBytesToPublic(byte[] zPublicKey) throws InvalidKeySpecException, NoSuchAlgorithmException, NoSuchProviderException {
		
		KeyFactory kf 			= KeyFactory.getInstance(ASYMETRIC_ALGORITHM,CRYPTO_PROVIDER);
		
		PublicKey publicKey 	= kf.generatePublic(new X509EncodedKeySpec(zPublicKey));
		
		return publicKey;
	}
	
	public static PrivateKey convertBytesToPrivate(byte[] zPrivateKey) throws InvalidKeySpecException, NoSuchAlgorithmException, NoSuchProviderException {
		
		KeyFactory kf 			= KeyFactory.getInstance(ASYMETRIC_ALGORITHM,CRYPTO_PROVIDER);
		
		PrivateKey privateKey 	= kf.generatePrivate(new PKCS8EncodedKeySpec(zPrivateKey));
		
		return privateKey;
	}
	
	public static byte[] secretKey() throws Exception {
    	
		KeyGenerator generator = KeyGenerator.getInstance(SYMETRIC_ALGORITHM,CRYPTO_PROVIDER);
    	generator.init(128); // The AES key size in number of bits
    	
    	SecretKey secKey = generator.generateKey();
    	
    	return secKey.getEncoded();
    }
	
	public static SecretKey convertSecret(byte[] zSecret) {
		return new SecretKeySpec(zSecret, SYMETRIC_ALGORITHM);
	}
	
	public static Cipher getAsymetricCipher() throws Exception {
		return Cipher.getInstance(ASYMETRIC_ALGORITHM, CRYPTO_PROVIDER);
	}
	
	public static Cipher getSymetricCipher() throws Exception {
		return Cipher.getInstance(SYMETRIC_ALGORITHM, CRYPTO_PROVIDER);
	}
	
}
