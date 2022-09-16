package org.minima.utils.encrypt.javajs;

import java.nio.charset.StandardCharsets;
import java.security.DigestException;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.security.Security;
import java.util.Arrays;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class JSEncrypt {

//	private static SecretKeySpec secretKey;
//    private static byte[] key;
// 
//    public static String decrptyBySyymetricKey(String encryptedSek, byte[] appKey) {
//        Key aesKey = new SecretKeySpec(appKey, "AES"); // converts bytes(32 byte random generated) to key
//        
//        try {
//            Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding"); // encryption type = AES with padding PKCS5
//            cipher.init(Cipher.DECRYPT_MODE, aesKey); // initiate decryption type with the key
//        
//            byte[] encryptedSekBytes = Base64.getDecoder().decode(encryptedSek); // decode the base64 encryptedSek to bytes
//        
//            byte[] decryptedSekBytes = cipher.doFinal(encryptedSekBytes); // decrypt the encryptedSek with the initialized cipher containing the key(Results in bytes)
//        
//            String decryptedSek = Base64.getEncoder().encodeToString(decryptedSekBytes); // convert the decryptedSek(bytes) to Base64 StriNG
//            return decryptedSek; // return results in base64 string
//        }catch(Exception e) {
//            return "Exception; "+e;
//        }
//    }
//    
//    public static void main(String[] args) throws Exception {
//
//    	byte[] data = "The data to encrypt".getBytes();
//    	
//    	byte[] secret 		= GenerateKey.secretKey();
//    	
//    	String secretstring = Base64.getEncoder().encodeToString(secret);
//    	
//    	System.out.println(secretstring);
//    	
////    	Key aesKey = new SecretKeySpec("r16glPt7vyO6g22KH4JcpzUIdnUXIy5p".getBytes(), "AES");
//    	Key aesKey = new SecretKeySpec(secret, "AES");
//    	
//    	
//    	//ENCRYPT
//    	byte[] encrypteddata;
//    	
//    	Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding"); // encryption type = AES with padding PKCS5
//        cipher.init(Cipher.ENCRYPT_MODE, aesKey); // initiate decryption type with the key
//    
//        encrypteddata = cipher.doFinal(data); 
//    	
//        String encstring = Base64.getEncoder().encodeToString(encrypteddata);
//    	
//        System.out.println(encstring);
//    
//        //DECRYPT
//        byte[] decrypteddata;
//    
//        Cipher cipherdec = Cipher.getInstance("AES/ECB/PKCS5Padding"); // encryption type = AES with padding PKCS5
//        cipherdec.init(Cipher.DECRYPT_MODE, aesKey); // initiate decryption type with the key
//    
//        decrypteddata = cipherdec.doFinal(encrypteddata); 
//    
//        String decstring = new String(decrypteddata);
//        
//        System.out.println(decstring);
//    	
//    }
	
	public static byte[][] generateKeyAndIV(int keyLength, int ivLength, int iterations, byte[] salt, byte[] password, MessageDigest md) {

        int digestLength = md.getDigestLength();
        int requiredLength = (keyLength + ivLength + digestLength - 1) / digestLength * digestLength;
        byte[] generatedData = new byte[requiredLength];
        int generatedLength = 0;

        try {
            md.reset();

            // Repeat process until sufficient data has been generated
            while (generatedLength < keyLength + ivLength) {

                // Digest data (last digest if available, password data, salt if available)
                if (generatedLength > 0)
                    md.update(generatedData, generatedLength - digestLength, digestLength);
                md.update(password);
                if (salt != null)
                    md.update(salt, 0, 8);
                md.digest(generatedData, generatedLength, digestLength);

                // additional rounds
                for (int i = 1; i < iterations; i++) {
                    md.update(generatedData, generatedLength, digestLength);
                    md.digest(generatedData, generatedLength, digestLength);
                }

                generatedLength += digestLength;
            }

            // Copy key and IV into separate byte arrays
            byte[][] result = new byte[2][];
            result[0] = Arrays.copyOfRange(generatedData, 0, keyLength);
            if (ivLength > 0)
                result[1] = Arrays.copyOfRange(generatedData, keyLength, keyLength + ivLength);

            return result;

        } catch (DigestException e) {
            throw new RuntimeException(e);

        } finally {
            // Clean out temporary data
            Arrays.fill(generatedData, (byte)0);
        }
    }
	
    public static void main(String args[]) {
        Security.addProvider(new BouncyCastleProvider());
        System.out.println(encrypt());
    }

    public static String encrypt() {
        try {
            String stringToEncrypt = "Hello world 12345678";
            String password = "apasswordblabla";
            
            SecureRandom sr = new SecureRandom();
            byte[] salt = new byte[8];
            sr.nextBytes(salt);
            final byte[][] keyAndIV = generateKeyAndIV(32, 16, 1, salt, password.getBytes(StandardCharsets.UTF_8),
                    MessageDigest.getInstance("MD5"));
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS7Padding", BouncyCastleProvider.PROVIDER_NAME);
            cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(keyAndIV[0], "AES"), new IvParameterSpec(keyAndIV[1]));
            byte[] encryptedData = cipher.doFinal(stringToEncrypt.getBytes(StandardCharsets.UTF_8));
            byte[] prefixAndSaltAndEncryptedData = new byte[16 + encryptedData.length];
            // Copy prefix (0-th to 7-th bytes)
            System.arraycopy("Salted__".getBytes(StandardCharsets.UTF_8), 0, prefixAndSaltAndEncryptedData, 0, 8);
            // Copy salt (8-th to 15-th bytes)
            System.arraycopy(salt, 0, prefixAndSaltAndEncryptedData, 8, 8);
            // Copy encrypted data (16-th byte and onwards)
            System.arraycopy(encryptedData, 0, prefixAndSaltAndEncryptedData, 16, encryptedData.length);
            return Base64.getEncoder().encodeToString(prefixAndSaltAndEncryptedData);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
	
}
