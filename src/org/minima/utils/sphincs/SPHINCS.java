package org.minima.utils.sphincs;

import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.objects.mmr.MMRData;
import org.minima.utils.Crypto;
import org.minima.utils.sphincs.FORS.FORS;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class SPHINCS {

	/**
	 * The seeds for both parts 
	 */
	private MiniData WOTS_SEED;
	private MiniData FORS_BASE_SEED;
	
	/**
	 * The WOTS key depth and keys per level
	 */
	private final int WOTS_DEPTH 		= 4;
	private final int WOTS_KEYSPERLEVEL = 192;
	
	/**
	 * The Public Key - is the root of the WOTS tree
	 */
	private MiniData SPHINCS_PUBLIC_KEY;
	
	/**
	 * The Private Key - Concatenation of the WOTS and FORS seeds
	 */
	private MiniData SPHINCS_PRIVATEKEY;
	
	/**
	 * How many LEAF nodes are there in the WOTS key
	 */
	BigInteger WOTS_KEY_NUM;
	
	/**
	 * Set up SPHINCS private keys from a seed
	 */
	public SPHINCS() {}
	
	public SPHINCS(MiniData zSeed) {
		initSeed(zSeed);
	}
	
	public void initSeed(MiniData zSeed) {
		/*
		 * Need to create 2*32 byte - one for the WOTS and one for the Random numbers in FORS
		 * 
		 * USE THGE ORIGINAL SEED as just hashing the private key will mean if you crack the first you get the second..
		 */
		MiniData wotsextra = new MiniData(new String("WOTS").getBytes());  
		MiniData wotsseed  = zSeed.concat(wotsextra);
		WOTS_SEED = new MiniData(Crypto.getInstance().hashData(wotsseed.getBytes()));
		
		MiniData forsextra = new MiniData(new String("FORS").getBytes());  
		MiniData forsseed = zSeed.concat(forsextra);
		FORS_BASE_SEED = new MiniData(Crypto.getInstance().hashData(forsseed.getBytes()));
	
		SPHINCS_PRIVATEKEY = WOTS_SEED.concat(FORS_BASE_SEED);
		
		//To get the public key create a TreeKey
		TreeKey wotskey 	= new TreeKey(WOTS_SEED, WOTS_KEYSPERLEVEL, WOTS_DEPTH);
		SPHINCS_PUBLIC_KEY 	= wotskey.getPublicKey();
		WOTS_KEY_NUM 		= new BigInteger(""+wotskey.getMaxUses());
	}
	
	/**
	 * Set the details from the privatekey
	 */
	public void initPrivateKey(MiniData zPrivateKey) {
		
		//Store this
		SPHINCS_PRIVATEKEY = zPrivateKey;
		
		//Chop key up into 2 parts..
		byte[] privkey = SPHINCS_PRIVATEKEY.getBytes();
		byte[] wotsbytes = new byte[32];
		byte[] forsbytes = new byte[32];
		for(int i=0;i<32;i++) {
			wotsbytes[i] = privkey[i];
			forsbytes[i] = privkey[i+32];
		}
		
		//Set vars
		WOTS_SEED 		= new MiniData(wotsbytes);
		FORS_BASE_SEED	= new MiniData(forsbytes);
		
		TreeKey wotskey 	= new TreeKey(WOTS_SEED, WOTS_KEYSPERLEVEL, WOTS_DEPTH);
		SPHINCS_PUBLIC_KEY 	= wotskey.getPublicKey();
		WOTS_KEY_NUM 		= new BigInteger(""+wotskey.getMaxUses());
	}
	
	public MiniData getPublicKey() {
		return SPHINCS_PUBLIC_KEY;
	}
	
	public MiniData getPrivateKey() {
		return SPHINCS_PRIVATEKEY;
	}
	
	public BigInteger getTotalWotsKeys() {
		return WOTS_KEY_NUM;
	}
	
	public static SPHINCSSignature signMessage(MiniData zMessage, MiniData zPrivateKey) {
		SPHINCS sphincs = new SPHINCS();
		sphincs.initPrivateKey(zPrivateKey);
		
		return sphincs.signMessage(zMessage); 
	}
	
	public SPHINCSSignature signMessage(MiniData zMessage) {
		
		/*
		 * Use the SAME WOTS KEY LEAF given a specific message - it ONLY signs the route of the FORS tree
		 * 
		 * So a given WOTS key always signs the same data
		 */
		MiniData hashmessage = new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Now do a modulo to get a value inside the wots key num..
		BigInteger keyval 	 = hashmessage.getDataValue().mod(WOTS_KEY_NUM);
		
		//THIS is the key to use..
		int keyuse 			 = keyval.intValueExact();
		
		//Now create a TREE key..
		TreeKey treekey = new TreeKey(WOTS_SEED, WOTS_KEYSPERLEVEL, WOTS_DEPTH);
		
		//Set the correct key
		treekey.setUses(keyuse);
		
		//Create the MinDara version
		MiniData keysdata = new MiniData(""+keyuse);
		
		//Now create a FORS tree with a UNIQUE seed - based on base FORS seed + Position (so is the same per key)
		MiniData prfunique 		= FORS_BASE_SEED.concat(keysdata); 
		MiniData uniqueforsseed = new MiniData(Crypto.getInstance().hashData(prfunique.getBytes()));
		
		//Now you can create the FORS Key
		FORS fors = new FORS(uniqueforsseed);
		
		//Get the root of the FORS tree
		MMRData rootforspublickey = fors.getForsRoot();
		
		//Sign that! - the SUM value is always the same 
		Signature wotssig = treekey.sign(rootforspublickey.getData());
		
		//Now sign the MESSAGE with the fors
		FORSSignature forssig = fors.signMessage(zMessage);
		
		//And create..
		SPHINCSSignature sig = new SPHINCSSignature(wotssig, rootforspublickey, forssig);
		
		return sig;
	}
	
	public static boolean verifySignature(MiniData zMessage, SPHINCSSignature zSignature, MiniData zSPHINCSPublicKey) {
		
		//The FORS root
		MMRData forsroot = zSignature.getFORSRoot();
		
		//The FORS signature
		FORSSignature forssig = zSignature.getFORSSignature();
		
		//First check the WOTS signature has signed the FORS tree root..
		Signature wotssig = zSignature.getWOTSSignature();
		
		//Create  treeKey
		TreeKey treekey 	= new TreeKey();
		treekey.setPublicKey(zSPHINCSPublicKey);
		if(!treekey.verify(forsroot.getData(), wotssig)) {
			log("SPHINCS VERIFY wots  : FALSE");
			return false;
		}
		
		//Now check the actual message against FORS
		if(!FORS.verifySignature(zMessage, forssig, forsroot)) {
			log("SPHINCS VERIFY fors  : FALSE");
			return false;
		}
		
		return true;
	}
	
	public static void log(String zMessage) {
		System.out.println(zMessage);
	}
	
	public static void main(String[] zArgs) {
		
		MiniData seed 	 = new MiniData("0x0011223344"); 
		MiniData message = new MiniData("0x998877661");
		
		log("Start SPHINCS..");
		
		SPHINCS sphincs = new SPHINCS();
		sphincs.initSeed(seed);
		
		log("SPHINCS public key : "+sphincs.getPublicKey().to0xString());
		log("SPHINCS total WOTS keys : "+sphincs.getTotalWotsKeys());
		
		SPHINCSSignature sig = sphincs.signMessage(message);
		
		//Write to stream
		MiniData stream = MiniData.getMiniDataVersion(sig); 
		
		//Read from stream
		SPHINCSSignature readsig = SPHINCSSignature.convertMiniDataVersion(stream);
		log("Signature Size : "+stream.getLength());
				
		//Verify
		boolean verify = SPHINCS.verifySignature(message, readsig, sphincs.getPublicKey());
		log("Verify : "+ verify);
	}
}
