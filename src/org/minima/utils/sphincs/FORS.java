package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMR;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMREntryNumber;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.Crypto;

public class FORS {

	MMR mForestHORS;
	
	HORST[] mHORSTLeafNodes;
	
	public FORS(MiniData zSeed) {
		
		//The Main FORS tree
		mForestHORS = new MMR();
		
		//Create the HORST trees for the leaf nodes
		mHORSTLeafNodes = new HORST[HORST.NUMBER_OF_CHUNK_BYTES];
		
		//Add a HORS tree as Leaf nodes
		for(int i=0;i<HORST.NUMBER_OF_CHUNK_BYTES;i++) {
			
			//Create a PseudoRandom Seed
			MiniData horsseed = Crypto.getInstance().hashAllObjects(zSeed, new MiniNumber(i));
			
			//Create a leaf node
			mHORSTLeafNodes[i] = new HORST(horsseed);
			
			//Get the root of that tree
			MMRData root = mHORSTLeafNodes[i].getHORSTRoot();
			
			//Now create a leaf node..
			MMRData leaf = MMRData.CreateMMRDataLeafNode(root.getData(), new MiniNumber(i));
			mForestHORS.addEntry(leaf);
		}
		
		mForestHORS.finalizeSet();
	}
	
	public MMRData getFORSRoot() {
		return mForestHORS.getRoot();
	}
	
	public FORSSignature signMessage(MiniData zMessage) {
		
		//Hash the message - 32 bytes
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Start a new signature
		FORSSignature sig = new FORSSignature();
		
		//Calculate all the signature values
		for(int i=0;i<HORST.NUMBER_OF_CHUNK_BYTES;i++) {
			
			//Get the HORST Tree
			HORST horst = mHORSTLeafNodes[i];
			
			//Get the value reference of the message
			int ref = HORST.getKeyRef(i, hm);
			
			//Add the simple Signature / Private key value (pre-image of public key)
			sig.getSignatureValues().add(horst.getPrivateKey().getKey(ref));
			
			//Add the Public key proof
			sig.getPublicKeyTreeProofs().add(horst.getPublicKey().getKeyTreeProof(ref));
			
			//Add the root of the HORST Tree
			MMRData horstroot = horst.getHORSTRoot();
			sig.getHORSTRoots().add(horstroot);
			
			//And finally add a proof of the HORST Tree!
			MMRProof horstprrof = mForestHORS.getProof(new MMREntryNumber(i));
			sig.getHORSTTreeProofs().add(horstprrof);
		}
		
		return sig;
	}
	
	public static boolean verifySignature(MiniData zMessage, FORSSignature zSignature, MMRData zFORSTreeRoot) {
		
		//Hash the message - 32 bytes
		MiniData hm	= new MiniData(Crypto.getInstance().hashData(zMessage.getBytes()));
		
		//Calculate all the signature values
		for(int i=0;i<HORST.NUMBER_OF_CHUNK_BYTES;i++) {
		
			//Get the horst root..
			MMRData horstroot = zSignature.getHORSTRoots().get(i);
			
			//Check the correct HORS tree is used
			MMRData horstleaf 		= MMRData.CreateMMRDataLeafNode(horstroot.getData(), new MiniNumber(i));
			MMRData checkhorstroot 	= zSignature.getHORSTTreeProofs().get(i).calculateProof(horstleaf);
			if(!checkhorstroot.isEqual(zFORSTreeRoot)) {
				return false;
			}
			
			//Get the value reference of the message
			int ref = HORST.getKeyRef(i, hm);
		
			//Get the signature / private key value
			MiniData privkey = zSignature.getSignatureValues().get(i);
			
			//Hash that to get the public key value
			MiniData checkpreimage = new MiniData(Crypto.getInstance().hashData(privkey.getBytes()));
			
			//Now check this is in the public key tree at the correct position
			MMRData leaf 		= MMRData.CreateMMRDataLeafNode(checkpreimage, new MiniNumber(ref));
			MMRData checkroot 	= zSignature.getPublicKeyTreeProofs().get(i).calculateProof(leaf);
			if(!checkroot.isEqual(horstroot)) {
				return false;
			}
		}
		
		return true;
	}
}
