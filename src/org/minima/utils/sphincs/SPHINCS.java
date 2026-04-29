package org.minima.utils.sphincs;

import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.utils.Crypto;

public class SPHINCS {

	/**
	 * The seeds for both parts 
	 */
	private MiniData WOTS_SEED;
	private MiniData FORS_BASE_SEED;
	
	/**
	 * The WOTS key depth and keys per level
	 */
	private final int WOTS_DEPTH 		= 3;
	private final int WOTS_KEYSPERLEVEL = 8;
	
	/**
	 * The Public Key - is the root of the WOTS tree
	 */
	private MiniData PUBLIC_KEY;
	
	/**
	 * How many LEAF nodes are there in the WOTS key
	 */
	int WOTS_KEY_NUM;
	
	/**
	 * Set up SPHINCS private keys from a seed
	 */
	public SPHINCS(MiniData zSeed) {
		
		/*
		 * Need to create 2*32 byte - one for the WOTS and one for the Random numbers in FORS
		 * 
		 * USE THGE ORIGINAL SEED as just hashing the private key will mean if you crack the first you get the second..
		 */
		MiniData wotsseed = zSeed.concat(new MiniData("0xFFEEDD99"));
		WOTS_SEED = new MiniData(Crypto.getInstance().hashData(wotsseed.getBytes()));
		
		MiniData forsseed = zSeed.concat(new MiniData("0xDDEEAADD"));
		FORS_BASE_SEED = new MiniData(Crypto.getInstance().hashData(forsseed.getBytes()));
	
		//To get the public key create a TreeKey
		TreeKey wotskey 	= new TreeKey(WOTS_SEED, WOTS_KEYSPERLEVEL, WOTS_DEPTH);
		PUBLIC_KEY 			= wotskey.getPublicKey();
		WOTS_KEY_NUM 		= wotskey.getMaxUses();
	}
	
	public MiniData getPublicKey() {
		return PUBLIC_KEY;
	}
	
	public int getTotalWotsKeys() {
		return WOTS_KEY_NUM;
	}
	
	public SPHINCSSignature signMessage(MiniData zMessage) {
		
		/*
		 * Use the SAME WOTS KEY LEAF given a specific message - it ONLY signs the route of the FORS tree
		 * 
		 * So a given WOTS key always signs the same data
		 */
		MiniData hm		= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		byte[] hmbytes 	= hm.getBytes();
		
		//Now get the first 8 bytes..
		byte[] keychoose = new byte[4];
		for(int i=0;i<4;i++) {
			keychoose[i] = hmbytes[i];
		}
		MiniData croppedhm = new MiniData(keychoose);
		
		//Now do a modulo to get a value inside the wots key num..
		BigInteger totalwots = new BigInteger(""+WOTS_KEY_NUM);
		BigInteger val 		 = croppedhm.getDataValue();
		BigInteger keyval 	 = val.mod(totalwots);
		
		//THIS is the key to use..
		int keyuse = keyval.intValueExact();
		log("Sphincs sign val:"+val+" keyuse:"+keyuse);
		
		//Now create a TREE key..
		TreeKey treekey = new TreeKey(WOTS_SEED, WOTS_KEYSPERLEVEL, WOTS_DEPTH);
		
		//Set the correct key
		treekey.setUses(keyuse);
		
		//Now create a FORS tree with a UNIQUE seed - based on base FORS seed + Position (so is the same per key)
		MiniData prfunique 		= FORS_BASE_SEED.concat(hm); 
		MiniData uniqueforsseed = new MiniData(Crypto.getInstance().hashData(prfunique.getBytes()));
		
		log("FORS key:"+uniqueforsseed.to0xString());
		
		//Now you can create the FORS Key
		FORS fors = new FORS(uniqueforsseed);
		
		//Get the root of the FORS tree
		MiniData forspublickey = fors.getForsRoot().getData();
		
		//Sign that!
		Signature wotssig = treekey.sign(forspublickey);
		
		//Now sign the MESSAGE with the fors
		FORSSignature forssig = fors.signMessage(zMessage);
		
		//And create..
		SPHINCSSignature sig = new SPHINCSSignature(wotssig,forssig);
		
		return sig;
	}
	
	public static boolean verifySignature(MiniData zMessage, SPHINCSSignature zSignature, MiniData zPublicKey) {
		
		
		
		return true;
	}
	
	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static void main(String[] zArgs) {
		
		MiniData seed 	 = new MiniData("0x0011223344"); 
		MiniData message = new MiniData("0x998877661");
		
		System.out.println("Start SPHINCS..");
		
		SPHINCS sphincs = new SPHINCS(seed);
		
		System.out.println("SPHINCS public key : "+sphincs.getPublicKey().to0xString());
		System.out.println("SPHINCS total WOTS keys : "+sphincs.getTotalWotsKeys());
		
		sphincs.signMessage(message);
		
	}
}
