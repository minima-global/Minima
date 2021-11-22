package org.minima.objects.keys;

import java.util.ArrayList;
import java.util.Collections;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;

public class TreeKey {

	public static final int DEFAULT_KEYSPERLEVEL = 16;
	public static final int DEFAULT_LEVELS 		 = 2;
	
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
	
	public TreeKey() {
		super();
	}
	
	public TreeKey(MiniData zPrivateSeed, int zKeyNum, int zLevels) {
		super();
		
		//Levels and Keys
		mLevels 		= zLevels;
		mKeysPerLevel 	= zKeyNum;
		
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
	
	public int getMaxUses() {
		return mMaxUses;
	}
	
	public int getUses() {
		return mUses;
	}
	
	public void setUses(int zUses) {
		mUses = zUses;
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
	
	
	public static void main(String[] zArgs) {
		
		MiniData seed = new MiniData("0x000102");
		
		int maxsigs = 5;
		
		TreeKey kt = new TreeKey(seed, 2, 3);
		
		//Set the pub key
		MiniData pk = kt.getPublicKey();
		
		MiniData data = new MiniData("0xFF");
//		MiniData data = MiniData.getRandomData(32);
//		MinimaLogger.log("DATA "+data.to0xString(32));
		
		TreeKey ktverify = new TreeKey();
		ktverify.setPublicKey(pk);
		
		for(int i=0;i<maxsigs;i++) {
			System.out.println();
			Signature sig = kt.sign(data);
			
			MiniData sigdata = MiniData.getMiniDataVersion(sig);
			System.out.println("Sig "+i+" : "+sigdata.to0xString(32)+" "+sigdata.getLength());
			
			//And verify..
			MinimaLogger.log("VERIFY : "+ktverify.verify(data, sig));
		}
		
//		MinimaLogger.log(convertBase(10, 16).toString());
		
//		MinimaLogger.log(baseConversion(new MiniNumber(27), 29, 4).toString());
		
		
	}
}
