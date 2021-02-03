package org.minima.utils.encryption;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;

public class CryptoPackage implements Streamable {

	MiniData mSecret;
	MiniData mData;
	
	public CryptoPackage() {}
	
	/**
	 * Encrypt data by creating an AES secret and encrpyt with this RSA key
	 * and then encrypt the actual data
	 * @param zData
	 * @param zPublicKey
	 * @throws Exception 
	 */
	public void encrypt(byte[] zData, byte[] zRSAPublicKey) throws Exception {
		//Create an AES 256 bit key
		byte[] secret = EncryptDecrypt.secretKey();
    	
		//Now encrypt the data with the secret
		byte[] encrypteddata = EncryptDecrypt.encryptSYM(secret, zData);
		mData = new MiniData(encrypteddata);
		
    	//Encrypt it with the Public Key
    	byte[] encryptedsecret = EncryptDecrypt.encryptASM(zRSAPublicKey, secret);
    	mSecret = new MiniData(encryptedsecret);
	}
	
	public byte[] decrypt(byte[] zPrivateKey) throws Exception {
		//First decrypt the secret
		byte[] secret = EncryptDecrypt.decryptASM(zPrivateKey, mSecret.getData());
		
		//Now decrypt the data
		byte[] dec = EncryptDecrypt.decryptSYM(secret, mData.getData());
		
		return dec;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mSecret.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mSecret = MiniData.ReadFromStream(zIn);
		mData = MiniData.ReadFromStream(zIn);
	}
	
	public static void main() {
		
	}
	
}
