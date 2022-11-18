package org.minima.utils.encrypt;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;

import org.minima.objects.base.MiniData;

public class PasswordCrypto {
    
    public static MiniData encryptPassword(String zPassword, MiniData zData) throws Exception {
    	
    	//The array we write data to
    	ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
    	DataOutputStream dos 		= new DataOutputStream(baos);
    	
    	//Generate Salt
    	MiniData salt 		=  MiniData.getRandomData(8);
    	
    	//Generate an IVParam
    	MiniData ivparam 	=  new MiniData(GenerateKey.IvParam());
    	
    	//Now write these 2 bits of info to the stream..
		salt.writeDataStream(dos);
		ivparam.writeDataStream(dos);
		
    	//Create an AES SecretKey with Password and Salt
		byte[] secret = GenerateKey.secretKey(zPassword,salt.getBytes()).getEncoded();
    	
		//Generate the Cipher
		Cipher ciph = GenerateKey.getCipherSYM(Cipher.ENCRYPT_MODE, ivparam.getBytes(), secret);
		CipherOutputStream cos 		= new CipherOutputStream(dos, ciph);
		GZIPOutputStream gzos		= new GZIPOutputStream(cos);
		DataOutputStream ciphdos 	= new DataOutputStream(gzos);
		
		//Now write out
		zData.writeDataStream(ciphdos);
		
		//Now close
		ciphdos.flush();
		ciphdos.close();
		
		gzos.close();
		cos.close();
		dos.close();
		
		return new MiniData(baos.toByteArray());
    }
    
    public static MiniData decryptPassword(String zPassword, MiniData zEncryptedData) throws Exception {
    	
    	//The input data array
    	ByteArrayInputStream bais 	= new ByteArrayInputStream(zEncryptedData.getBytes());
    	DataInputStream dis 		= new DataInputStream(bais);
    	
    	//Read the salt
    	MiniData salt = MiniData.ReadFromStream(dis);
    	
    	//Read the IVParam
    	MiniData ivparam = MiniData.ReadFromStream(dis);
    	
    	//Create an AES SecretKey with Password and Salt
		byte[] secret = GenerateKey.secretKey(zPassword,salt.getBytes()).getEncoded();
    	
		//Generate the Cipher
		Cipher ciph = GenerateKey.getCipherSYM(Cipher.DECRYPT_MODE, ivparam.getBytes(), secret);
		CipherInputStream cis 	= new CipherInputStream(dis, ciph);
		GZIPInputStream gzin 	= new GZIPInputStream(cis);
		DataInputStream cipdis  = new DataInputStream(gzin);
		
		//Now read in the data
		MiniData decrypted = MiniData.ReadFromStream(cipdis);
		
		//Now close
		cipdis.close();
		gzin.close();
		cis.close();
		dis.close();
    	
    	return decrypted;
    }
    
    public static void main(String[] zArgs) throws Exception {
    	
    	MiniData data 	= MiniData.getRandomData(32);
    	System.out.println(data.to0xString());
		
    	String password = "apasswordblabla";
    	
    	MiniData enc = encryptPassword(password, data);
    	System.out.println(enc.getLength());
		System.out.println(enc.to0xString());
    	
		MiniData dec = decryptPassword(password, enc);
		System.out.println(dec.getLength());
		System.out.println(dec.to0xString());
    }    
}