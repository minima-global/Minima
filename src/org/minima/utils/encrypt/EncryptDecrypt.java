package org.minima.utils.encrypt;

import java.security.PrivateKey;
import java.security.PublicKey;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

import org.minima.objects.base.MiniData;

public class EncryptDecrypt {
	
    public static byte[] encryptASM(byte[] zPublicKey, byte[] inputData) throws Exception {

        PublicKey key = GenerateKey.convertBytesToPublic(zPublicKey);
        
        Cipher cipher = GenerateKey.getAsymetricCipher();
        cipher.init(Cipher.ENCRYPT_MODE, key);

        return cipher.doFinal(inputData);
    }

    public static byte[] decryptASM(byte[] zPrivateKey, byte[] encryptedData) throws Exception {

        PrivateKey key 	= GenerateKey.convertBytesToPrivate(zPrivateKey);

        Cipher cipher 	= GenerateKey.getAsymetricCipher();
        cipher.init(Cipher.DECRYPT_MODE, key);

        return cipher.doFinal(encryptedData);
    }
    
    public static byte[] encryptSYM(byte[] zIvParam, byte[] zSecretKey, byte[] inputData) throws Exception {
    	
    	SecretKey sk 		= GenerateKey.convertSecret(zSecretKey);    	
    	IvParameterSpec iv 	= new IvParameterSpec(zIvParam);
    	
    	Cipher aesCipher 	= GenerateKey.getSymetricCipher();
		aesCipher.init(Cipher.ENCRYPT_MODE, sk, iv);
		
    	return aesCipher.doFinal(inputData);
    }
    
    public static byte[] decryptSYM(byte[] zIvParam, byte[] zSecretKey, byte[] encryptedData) throws Exception {
    	SecretKey sk 		= GenerateKey.convertSecret(zSecretKey);
    	IvParameterSpec iv 	= new IvParameterSpec(zIvParam);
    	
    	Cipher aesCipher 	= GenerateKey.getSymetricCipher();
		aesCipher.init(Cipher.DECRYPT_MODE, sk, iv);
		
    	return aesCipher.doFinal(encryptedData);
    }
    
    public static void main(String[] args) throws Exception {

    	//ASYMMETRIC example..
//        KeyPair generateKeyPair = generateKeyPair();
//        byte[] publicKey = generateKeyPair.getPublic().getEncoded();
//        byte[] privateKey = generateKeyPair.getPrivate().getEncoded();
        MiniData pubk = new MiniData("0x30819F300D06092A864886F70D010101050003818D0030818902818100C62066FF7598C930630E0CBFE9D067677F41075EC817CDFE807619F9A4AAC510C2FC82E398C0DBA126F562FC05031ECE32ED2B7E92696AB7FCE0DA183BA93E8E441046323B40CB2049749B6B1278D0E7C1A4AB1D688E53C8869450C243534DDC350ADED0506E39F444609AA537E47FAC75FA209110FE3D2BF7ECCD386D1BC3750203010001");
//        System.out.println("PUB : "+pubk.getLength()+" "+pubk.to0xString());
        MiniData privk = new MiniData("0x30820277020100300D06092A864886F70D0101010500048202613082025D02010002818100C62066FF7598C930630E0CBFE9D067677F41075EC817CDFE807619F9A4AAC510C2FC82E398C0DBA126F562FC05031ECE32ED2B7E92696AB7FCE0DA183BA93E8E441046323B40CB2049749B6B1278D0E7C1A4AB1D688E53C8869450C243534DDC350ADED0506E39F444609AA537E47FAC75FA209110FE3D2BF7ECCD386D1BC375020301000102818001617DB78C0E778D20DE25005D81318F0BBE3AF4A041AA26F4D767047B6716039CCDFEDA0B45D6CAA864E02B70325C91A82BE36A1E74445829F10F65964CCBE524E3AE01CDD993F6C4B107B37CF582989ED99007CF1DB5D6EBF6D93B93FF376F06C9810142D402CFAAA045BCD5E3D9B907AA1227239954053AD8B9A166EDF457024100FA46F0E31EAB16D380F9C63BF86AB750DD3B5CC181CBD47CEEAD857D9A1182BAA6A1012BC93A118D492AB8D7499C728E7D08243FAF10FFF9458D9BC64E558AEB024100CAA82F6D974D47BF3A9E62A52F041F6EADEA704157E96E484CE9BD52F665CAB5CD9E80A8DEA799A70D9B643276FDCAA37B89A3A2D020AFE4E9F92679F131931F024100BECF49260F22A753771876CF0FF4E46FED4B2DCB86168D98F62B2B03F86B733CB47F39B25547454C6F44F80982E098E213671AA3C6E7F20FECBC2AD044B6A923024040254C1DDC2E51EF6D968492D476F19C0EE08DF7A0E3FA4C584C03E926A4C1AD1A107998FA2102B9A5CCF1C22777DD94319E3775697B6F317DFDA6F8222816F1024100C8BE7B5202FC8C171AF974EC3F49A97F3491BE764305623EC820C283495937DA65838A523D03524C8ED875529CECB1BE4568218B316253E0968444FE97D01168");
//        System.out.println("PRV : "+privk.to0xString());
        byte[] encryptedData = encryptASM(pubk.getBytes(),"HELLO - this is my message!!".getBytes());
        MiniData enc = new MiniData(encryptedData);
//        System.out.println("ENC : "+enc.to0xString());
        byte[] decryptedData = decryptASM(privk.getBytes(), encryptedData);
        System.out.println(new String(decryptedData));

    	
    }
}