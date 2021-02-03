package org.minima.utils.encryption;

import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class EncryptDecrypt {

	private static final String ASYMETRIC_ALGORITHM = "RSA";
	private static final String SYMETRIC_ALGORITHM  = "AES";

    public static byte[] encryptASM(byte[] publicKey, byte[] inputData) throws Exception {

        PublicKey key = KeyFactory.getInstance(ASYMETRIC_ALGORITHM)
                .generatePublic(new X509EncodedKeySpec(publicKey));

        Cipher cipher = Cipher.getInstance(ASYMETRIC_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, key);

        byte[] encryptedBytes = cipher.doFinal(inputData);

        return encryptedBytes;
    }

    public static byte[] decryptASM(byte[] privateKey, byte[] encryptedData) throws Exception {

        PrivateKey key = KeyFactory.getInstance(ASYMETRIC_ALGORITHM)
        		.generatePrivate(new PKCS8EncodedKeySpec(privateKey));

        Cipher cipher = Cipher.getInstance(ASYMETRIC_ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, key);

        byte[] decryptedBytes = cipher.doFinal(encryptedData);

        return decryptedBytes;
    }

    public static KeyPair generateKeyPair() throws Exception {

        KeyPairGenerator keyGen = KeyPairGenerator.getInstance(ASYMETRIC_ALGORITHM);

        //SecureRandom random = SecureRandom.getInstance("SHA1PRNG", "SUN");
        SecureRandom random = SecureRandom.getInstanceStrong();

        // 512 is keysize
        keyGen.initialize(2048, random);

        KeyPair generateKeyPair = keyGen.generateKeyPair();
        return generateKeyPair;
    }
    
    public static byte[] secretKey() throws NoSuchAlgorithmException {
    	KeyGenerator generator = KeyGenerator.getInstance(SYMETRIC_ALGORITHM);
    	generator.init(128); // The AES key size in number of bits
    	SecretKey secKey = generator.generateKey();
    	return secKey.getEncoded();
    }
    
    public static byte[] encryptSYM(byte[] secretKey, byte[] inputData) throws Exception {
    	SecretKey sk = new SecretKeySpec(secretKey, SYMETRIC_ALGORITHM);
    	Cipher aesCipher = Cipher.getInstance(SYMETRIC_ALGORITHM);
		aesCipher.init(Cipher.ENCRYPT_MODE, sk);
		byte[] byteCipherText = aesCipher.doFinal(inputData);
    	return byteCipherText;
    }
    
    public static byte[] decryptSYM(byte[] secretKey, byte[] encryptedData) throws Exception {
    	SecretKey sk = new SecretKeySpec(secretKey, SYMETRIC_ALGORITHM);
    	Cipher aesCipher = Cipher.getInstance(SYMETRIC_ALGORITHM);
		aesCipher.init(Cipher.DECRYPT_MODE, sk);
		byte[] byteCipherText = aesCipher.doFinal(encryptedData);
    	return byteCipherText;
    }
    
    public static void main(String[] args) throws Exception {

    	//ASYMMETRIC example..
//        KeyPair generateKeyPair = generateKeyPair();
//        byte[] publicKey = generateKeyPair.getPublic().getEncoded();
//        byte[] privateKey = generateKeyPair.getPrivate().getEncoded();
//        MiniData pubk = new MiniData(publicKey);
//        System.out.println("PUB : "+pubk.getLength()+" "+pubk.to0xString());
//        MiniData privk = new MiniData(privateKey);
//        System.out.println("PRV : "+privk.to0xString());
//        byte[] encryptedData = encryptASM(publicKey,"HELLO - this is my message!!".getBytes());
//        MiniData enc = new MiniData(encryptedData);
//        System.out.println("ENC : "+enc.to0xString());
//        byte[] decryptedData = decryptASM(privateKey, encryptedData);
//        System.out.println(new String(decryptedData));

//    	for (Provider provider: Security.getProviders()) {
//		  System.out.println(provider.getName());
//		  for (String key: provider.stringPropertyNames())
//		    System.out.println("\t" + key + "\t" + provider.getProperty(key));
//		}
	
//    	Security.addProvider(new BouncyCastleProvider());
//    	KeyGenerator generator = KeyGenerator.getInstance("AES","BC");
    	
    	
//    	//SYMMETRIC example
//    	byte[] secret = secretKey();
//    	MiniData sec = new MiniData(secret);
//    	System.out.println("SEC : "+sec.getLength()+" "+sec.to0xString());
//    	
//    	byte[] encrypted = encryptSYM(secret, "HEELO! - this is the message!! and there is no limit to the length you can encrypt..".getBytes());
//    	MiniData enc = new MiniData(encrypted);
//    	System.out.println("ENC : "+enc.to0xString());
//    	byte[] decrypt = decryptSYM(secret, encrypted);
//    	System.out.println("DEC : "+new String(decrypt));
    	
    	//Now decrypt..
    	
    	
    	
    }
}
