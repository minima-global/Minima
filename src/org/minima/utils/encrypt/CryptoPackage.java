package org.minima.utils.encrypt;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.KeyPair;
import java.security.Security;

import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;

public class CryptoPackage implements Streamable {

	/**
	 * The IvParam
	 */
	MiniData mIvParam;
	
	/**
	 * The secret that SYMMETRICALLY encrypts the data 
	 * but is asymmetrically encrypted itself with a public key
	 */
	MiniData mSecret;
	
	/**
	 * The data that is encrypted by the unencrypted secret
	 */
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
		
		//Create a IvParam for this round of encryoption
		byte[] ivparam = GenerateKey.IvParam();
		mIvParam = new MiniData(ivparam);
		
		//Create an AES key
		byte[] secret = GenerateKey.secretKey();
		
		//Now encrypt the data with the secret
		byte[] encrypteddata = EncryptDecrypt.encryptSYM(ivparam, secret, zData);
		mData = new MiniData(encrypteddata);
		
    	//Encrypt it with the Public Key
    	byte[] encryptedsecret = EncryptDecrypt.encryptASM(zRSAPublicKey, secret);
    	mSecret = new MiniData(encryptedsecret);
	}
	
	public byte[] decrypt(byte[] zRSAPrivateKey) throws Exception {
		//First decrypt the secret
		byte[] secret = EncryptDecrypt.decryptASM(zRSAPrivateKey, mSecret.getBytes());
		
		//Now decrypt the data
		byte[] dec = EncryptDecrypt.decryptSYM(mIvParam.getBytes(), secret, mData.getBytes());
		
		return dec;
	}
	
	/**
	 * Get the MiniData version of this Object
	 */
	public MiniData getCompleteEncryptedData() {
		return MiniData.getMiniDataVersion(this);
	}
	
	/**
	 * Convert a MiniData Version
	 */
	public void ConvertMiniDataVersion(MiniData zComplete) throws IOException {
		ByteArrayInputStream bais = new ByteArrayInputStream(zComplete.getBytes());
		DataInputStream dis = new DataInputStream(bais);
		readDataStream(dis);
		dis.close();
		bais.close();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mIvParam.writeDataStream(zOut);
		mSecret.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mIvParam	= MiniData.ReadFromStream(zIn);
		mSecret 	= MiniData.ReadFromStream(zIn);
		mData 		= MiniData.ReadFromStream(zIn);
	}
	
	public static CryptoPackage ReadFromStream(DataInputStream zIn) throws IOException {
		CryptoPackage crypt = new CryptoPackage();
		crypt.readDataStream(zIn);
		return crypt;
	} 
	
	public static void main(String[] zArgs) throws Exception {
		//We use Bouncy
		Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
		 		
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		MiniData pubk 			= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		MiniData privk 			= new MiniData(privateKey);
		
		MiniData rdata = MiniData.getRandomData(256);
		CryptoPackage cp = new CryptoPackage();
		cp.encrypt(rdata.getBytes(), publicKey);
		
		System.out.println("Public Key  : "+pubk.getLength());
		System.out.println("Private Key : "+privk.getLength());
		System.out.println("Secret Key  : "+cp.mSecret.getLength());
		System.out.println("Data        : "+rdata.getLength());
		System.out.println("Enc Data    : "+cp.getCompleteEncryptedData().getLength());
		
		byte[] dec = cp.decrypt(privateKey);
		MiniData decdata = new MiniData(dec);
		
		System.out.println("Worked     : "+decdata.isEqual(rdata));
		
		
		
	}
	
}
