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
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

public class GenerateKey {

	public static final String 	ASYMETRIC_ALGORITHM_GEN = "RSA";
	public static final String 	ASYMETRIC_ALGORITHM 	= "RSA/ECB/PKCS1Padding";
	
	private static final String SYMETRIC_ALGORITHM_GEN  = "AES";
	private static final String SYMETRIC_ALGORITHM  	= "AES/CBC/PKCS5Padding";
	
	private static final String SYMETRIC_PASSWORD_ALGORITHM  = "PBKDF2WithHmacSHA256";
	
	public static KeyPair generateKeyPair() throws Exception {

		SecureRandom random 	= new SecureRandom();
		
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance(ASYMETRIC_ALGORITHM_GEN);
        keyGen.initialize(1024, random);
        
        KeyPair generateKeyPair = keyGen.generateKeyPair();
        
        return generateKeyPair;
    }
	
	public static PublicKey convertBytesToPublic(byte[] zPublicKey) throws InvalidKeySpecException, NoSuchAlgorithmException, NoSuchProviderException {
		
		KeyFactory kf 			= KeyFactory.getInstance(ASYMETRIC_ALGORITHM_GEN);
		PublicKey publicKey 	= kf.generatePublic(new X509EncodedKeySpec(zPublicKey));
		
		return publicKey;
	}
	
	public static PrivateKey convertBytesToPrivate(byte[] zPrivateKey) throws InvalidKeySpecException, NoSuchAlgorithmException, NoSuchProviderException {
		
		KeyFactory kf 			= KeyFactory.getInstance(ASYMETRIC_ALGORITHM_GEN);
		PrivateKey privateKey 	= kf.generatePrivate(new PKCS8EncodedKeySpec(zPrivateKey));
		
		return privateKey;
	}
	
	public static byte[] secretKey() throws Exception {
    	
		KeyGenerator generator = KeyGenerator.getInstance(SYMETRIC_ALGORITHM_GEN);
    	generator.init(128); // The AES key size in number of bits
    	
    	SecretKey secKey = generator.generateKey();
    	
    	byte[] secret = secKey.getEncoded();
    	
    	return secret;
    }
	
	public static SecretKey convertSecret(byte[] zSecret) {
		return new SecretKeySpec(zSecret, SYMETRIC_ALGORITHM_GEN);
	}
	
	public static SecretKey secretKey(String zPassword, byte[] zSalt) throws Exception {
    	
		SecretKeyFactory factory 	= SecretKeyFactory.getInstance(SYMETRIC_PASSWORD_ALGORITHM);
		KeySpec spec 				= new PBEKeySpec(zPassword.toCharArray(), zSalt, 65536, 128);
		SecretKey tmp 				= factory.generateSecret(spec);
		SecretKey secret 			= new SecretKeySpec(tmp.getEncoded(), SYMETRIC_ALGORITHM_GEN);
    	
    	return secret;
    }
	
	public static byte[] IvParam() throws Exception {
    	
		SecureRandom random 	= new SecureRandom();
		byte[] ivBytes 			= new byte[16];
		random.nextBytes(ivBytes);
    	
    	return ivBytes;
    }
	
	public static Cipher getAsymetricCipher() throws Exception {
		return Cipher.getInstance(ASYMETRIC_ALGORITHM);
	}
	
	public static Cipher getSymetricCipher() throws Exception {
		return Cipher.getInstance(SYMETRIC_ALGORITHM);
	}
	
	public static Cipher getCipherSYM(int zCipherMode, byte[] zIvParam, byte[] zSecretKey) throws Exception {
    	
    	SecretKey sk 		= GenerateKey.convertSecret(zSecretKey);    	
    	IvParameterSpec iv 	= new IvParameterSpec(zIvParam);
    	
    	Cipher aesCipher 	= GenerateKey.getSymetricCipher();
		aesCipher.init(zCipherMode, sk, iv);
		
    	return aesCipher;
    }
}
