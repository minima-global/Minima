package org.minima.objects.keys;

import java.util.ArrayList;
import java.util.Collections;

import org.minima.kissvm.functions.sha.PROOF;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMR;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMREntryNumber;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;


public class TreeKey {

	/**
	 * Maximum Levels for any Key allowed
	 */
	public static final int MAX_KEY_LEVELS		 = 8;
	
	/**
	 * Default Values
	 */
	public static final int DEFAULT_KEYSPERLEVEL = 64;
	public static final int DEFAULT_LEVELS 		 = 3;
	
	public static TreeKey createDefault(MiniData zPrivateSeed) {
		return new TreeKey(zPrivateSeed, DEFAULT_KEYSPERLEVEL, DEFAULT_LEVELS);
	}
	
	/**
	 * The ROOT of the Tree of Keys
	 */
	TreeKeyNode mRoot;
	
	int mLevels;
	int mKeysPerLevel;
	int mUses;
	int mMaxUses;
	
	MiniData mPrivateSeed;
	MiniData mPublicKey;
	
	public TreeKey() {}
	
	public TreeKey(MiniData zPrivateSeed, int zKeyNum, int zLevels) {
		
		//Levels and Keys
		mLevels 		= zLevels;
		mKeysPerLevel 	= zKeyNum;
		
		//Check maximum
		if(mLevels>MAX_KEY_LEVELS) {
			throw new IllegalArgumentException("Too many key Levels "+mLevels+" MAX:"+MAX_KEY_LEVELS);
		}
		
		mUses			= 0; 
		mMaxUses 		= (int) Math.pow(mKeysPerLevel, mLevels);
		
		//Store..
		mPrivateSeed = zPrivateSeed;
				
		//Initialise root
		mRoot = new TreeKeyNode(zPrivateSeed, mKeysPerLevel);
		
		//Get the Public Key.,.
		mPublicKey = mRoot.getPublicKey();
	}
	
	public void setPublicKey(MiniData zPublicKey) {
		mPublicKey = zPublicKey;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateKey() {
		return mPrivateSeed;
	}
	
	public int getMaxUses() {
		return mMaxUses;
	}
	
	public int getUses() {
		return mUses;
	}
	
	public void setUses(int zUses) {
		mUses = zUses;
	}
	
	public int getSize() {
		return mKeysPerLevel;
	}
	
	public int getDepth() {
		return mLevels;
	}
	
	public Signature sign(MiniData zData) {
		
		//Check range
		if(mUses >= mMaxUses) {
			MinimaLogger.log("SERIOUS ERROR : MAX TREEKEYS USED @ "+mPublicKey);
			mUses = 0;
		}
		
		//Get the Correct Node path..
		ArrayList<Integer> nodes = baseConversion(mUses, mKeysPerLevel, mLevels);
//		MinimaLogger.log("KEY TREE SIGN : "+mUses+" "+nodes.toString());
		
		//All the signatures..
		Signature signature = new Signature();
		
		//Now get those Nodes..
		TreeKeyNode current = mRoot;
		int depth 			= 1;
		for(Integer node : nodes) {
			
			//The node..
			int keynum = node.intValue();
			
			//Get the required key
			Winternitz wots = current.getWOTSKey(keynum);
			
			//The Public Key
			MiniData sigpubkey = wots.getPublicKey(); 
			
			//Get the MMRProof..
			MMRProof proof = current.getProof(keynum);
			
			//Is this the final node
			if(depth == mLevels) {
				
				//Sign the actual Data
				MiniData sigdata = wots.sign(zData);
				
				//Create a signature object
				SignatureProof sig = new SignatureProof(sigpubkey, sigdata, proof);
				
				//Add it..
				signature.addSignatureProof(sig);
				
			}else {
				
				//Get the correct child node..
				TreeKeyNode child = current.getChild(keynum);
				
				//Do we need to sign it.. ( only need to do this once is reused multiple times )
				if(!child.childSigExists()) {
					
					//Get the child's Public Key
					MiniData data = child.getPublicKey();
					
					//Sign the root of the child tree
					MiniData sigdata = wots.sign(data);
				
					//Create the signature object
					SignatureProof childsig = new SignatureProof(sigpubkey, sigdata, proof);
					
					//Set it for next time..
					child.setParentChildSig(childsig);
				}
				
				//Get the parent child sig
				SignatureProof parentchild = child.getParentChildSig();
				
				//Add it..
				signature.addSignatureProof(parentchild);
				
				//New current node
				current = child;
			}
			
			depth++;
		}
		
		//One signature done
		mUses++;
		
		//Return that..
		return signature;
	}


	public boolean verify(MiniData zData, Signature zSignature) {
		
		//Cycle through..
		int total = zSignature.getAllSignatureProofs().size();
		if(total>MAX_KEY_LEVELS) {
			MinimaLogger.log("[!] INVALID KEY found with "+total+" levels MAX:"+MAX_KEY_LEVELS);
			return false;
		}
		
		for(int depth=0;depth<total;depth++) {
			
			//Get the signature
			SignatureProof sigproof = zSignature.getAllSignatureProofs().get(depth);
			
			//Check this root public key is the one we need
			if(depth == 0) {
				
				//Check this is the MAIN public Key
				if(!sigproof.getRootPublicKey().isEqual(mPublicKey)) {
					return false;
				}
			}

			//Is this the last Signature
			if(depth == total-1) {
				
				//The LAST signature signs the actual DATA
				return Winternitz.verify(sigproof.getPublicKey(), zData, sigproof.getSignature());
				
			}else {
				
				//Any Signature but the last signs the child root public key
				SignatureProof childsig = zSignature.getAllSignatureProofs().get(depth+1);
				
				//Check this is what is signed..
				if(!Winternitz.verify(sigproof.getPublicKey(), childsig.getRootPublicKey(), sigproof.getSignature())) {
					return false;
				}
			}
		}
		
		return false;
	}
	

	/**
	 * Base converter to tell which nodes in the tree to use..
	 */
	private static ArrayList<Integer> baseConversion(int zNum, int zBase, int zLevels){
		ArrayList<Integer> ret = new ArrayList<>();
		
		int counter = zNum;
		while(counter != 0) {
			int div 	= counter / zBase;
			int remain 	= counter - (div * zBase);
			ret.add(remain);
			counter = div;
		}
		
		//Do we have it..
		int sizediff = zLevels - ret.size();
		for(int i=0;i<sizediff;i++) {
			ret.add(0);
		}
		
		//Reverse
		Collections.reverse(ret);
		
		return ret;
	}
	
	//HORS
	public static MiniData shrinkData(int zBytes, MiniData zOrig) {
		
		byte[] orig = zOrig.getBytes();
		byte[] res 	= new byte[zBytes];
		for(int i=0;i<zBytes;i++) {
			res[i] = orig[i];
		}
		
		return new MiniData(res);
	}
	
	public static int getKeyRef(int zPos, MiniData zOrig) {
	
		byte[] allbytes	= zOrig.getBytes();
		int val 		= allbytes[zPos] & 0xFF;
		
		return val;
	}
	
	public static void main(String[] zArgs) {
		
		int tp 	= 8;
		int t	= (int)Math.pow(2, tp);
		int k	= 16;
		
		int hashlen = (tp * k) / 8;
		MinimaLogger.log("Private Key size : "+t);
		MinimaLogger.log("Hash Len         : "+hashlen+" ");
		
		MiniData[] privatekey = new MiniData[t];
		MiniData[] publickey  = new MiniData[t];
		
		MiniData privkeyseed = new MiniData("0xFFEEDD");
		
		MMR pubkeytree = new MMR();
		for(int i=0;i<t;i++) {
			privatekey[i] 	= Crypto.getInstance().hashAllObjects(privkeyseed, new MiniNumber(i));
			publickey[i] 	= new MiniData(Crypto.getInstance().hashData(privatekey[i].getBytes()));
		
			//Create an MMR of the public Key
			MMRData leaf = MMRData.CreateMMRDataLeafNode(publickey[i], new MiniNumber(i));
			pubkeytree.addEntry(leaf);
		}
		pubkeytree.finalizeSet();
		
		MMRData pubkeytreeroot = pubkeytree.getRoot();
		MinimaLogger.log("PUBLIC KEY ROOT HASH : "+pubkeytreeroot.getData().to0xString());
		MinimaLogger.log("PUBLIC KEY ROOT SUM  : "+pubkeytreeroot.getValue());
		
		//The Message
		MiniData message 	= new MiniData("0xFFEEDDFFEEDD");
		
		//First hash the message
		MiniData hm			= Crypto.getInstance().hashObject(message);
		MiniData shm	 	= shrinkData(hashlen, hm);
		
		MinimaLogger.log("Message : "+shm.getLength()+" "+shm.to0xString());
		
		//SIGNATURE CREATION
		MinimaLogger.log("");
		MinimaLogger.log("Signature:");
		MiniData[] sig 				= new MiniData[hashlen];
		MiniData[] sigpubkey		= new MiniData[hashlen];
		MMRProof[] sigprooftree 	= new MMRProof[hashlen];
		
		for(int i=0;i<hashlen;i++) {
		//for(int i=0;i<1;i++) {
			int ref = getKeyRef(i, shm);
			
			//The signature is the private key values..
			sig[i] = privatekey[ref];
			
			//Store the SigPubKey
			sigpubkey[i] = publickey[ref];
			
			//Get the ptree proof..
			sigprooftree[i] = pubkeytree.getProof(new MMREntryNumber(ref)); 
			
			//Simple Check..
			MMRData leaf = MMRData.CreateMMRDataLeafNode(publickey[ref], new MiniNumber(ref));
			MMRData root = sigprooftree[i].calculateProof(leaf);
			if(!root.getData().isEqual(pubkeytreeroot.getData())) {
				MinimaLogger.log("ProofTree Create  : FAIL CHECK!");
			}
			
			//LOG
			if(i<2) {
				MinimaLogger.log("");
				MinimaLogger.log("SIG POS       : "+i);
				MinimaLogger.log("SIG REF       : "+ref);
				MinimaLogger.log("SIG           : "+sig[i]);
				MinimaLogger.log("SIGPUBKEY     : "+sigpubkey[i]);
				MinimaLogger.log("SIGTREE       : "+MiniData.getMiniDataVersion(sigprooftree[i]).to0xString());
			}
		}
		
		/*
		 * VERIFY Code
		 * 
		 * Inputs.. the 160bit message
		 * 
		 * Sig and tree
		 * 
		 * And the root public key
		 */
		
		MinimaLogger.log("");
		MinimaLogger.log("Verify:");
		boolean valid = true;
		for(int i=0;i<hashlen;i++) {
			
			//Get the ref
			int ref = getKeyRef(i, shm);
			
			//Check the hash of the sig is the pub key provided
			MiniData check = new MiniData(Crypto.getInstance().hashData(sig[i].getBytes()));
			if(!check.isEqual(sigpubkey[i])) {
				MinimaLogger.log("Simple  : FAIL CHECK!");
				valid = false;
				break;
			}
			
			//Now check the sigpubkey is in the pubkeytree
			MMRData leaf 		= MMRData.CreateMMRDataLeafNode(sigpubkey[i], new MiniNumber(ref));
			MMRData checkroot 	= sigprooftree[i].calculateProof(leaf);
			if(!checkroot.getData().isEqual(pubkeytree.getRoot().getData())) {
				MinimaLogger.log("ProofTree  : FAIL CHECK!");
				valid = false;
				break;
			}
			
		}
		MinimaLogger.log("All checks Done! "+valid);
		
		
		/*MiniData seed 	= new MiniData("0x000102");
		
		long timestart 	= System.currentTimeMillis();
		System.out.println("Time Start key gen : "+timestart);
		
		TreeKey kt 	 	= new TreeKey(seed, 256, 4);
		MiniData pk 	= kt.getPublicKey();
		
		long timefinish = System.currentTimeMillis();
		System.out.println("Time Finish key gen: "+timestart);
		
		long timediff = timefinish - timestart;
		System.out.println("TimeDiff : "+timediff);
		
		//First Run
		MiniData data = MiniData.getRandomData(32);
		MinimaLogger.log("DATA "+data.to0xString(32));
		timestart 	= System.currentTimeMillis();
		System.out.println("Time Start sign : "+timestart);
		
		Signature sig = kt.sign(data);
		
		timefinish = System.currentTimeMillis();
		System.out.println("Time Finish sign : "+timefinish);
		
		timediff = timefinish - timestart;
		System.out.println("TimeDiff : "+timediff);
		
		//Second Run
		data = MiniData.getRandomData(32);
		MinimaLogger.log("");
		MinimaLogger.log("DATA (2)"+data.to0xString(32));
		timestart 	= System.currentTimeMillis();
		System.out.println("Time Start sign : "+timestart);
		
		sig = kt.sign(data);
		
		timefinish = System.currentTimeMillis();
		System.out.println("Time Finish sign : "+timefinish);
		
		timediff = timefinish - timestart;
		System.out.println("TimeDiff : "+timediff);
		*/
	}
}
