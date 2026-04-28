package org.minima.utils.sphincs;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;

public class Signature {

	/**
	 * These are the Private key values at the referenced position in the private key
	 * 
	 * The Pre-images of the Public key values..
	 */
	ArrayList<MiniData> mSigValues = new ArrayList<>();
	
	/**
	 * This is the root of the Public Key Tree
	 */
	MMRData mRoot;
	
	/**
	 * These are the MMRProofs of each public key value in the tree
	 */
	ArrayList<MMRProof>  mPublicKeyProofs = new ArrayList<>();
	
	public Signature() {}
	
	public ArrayList<MiniData> getSignatureValues(){
		return mSigValues;
	}
	
	public ArrayList<MMRProof> getPublicKeyTreeProofs(){
		return mPublicKeyProofs;
	}
	
	public void setPublicKeyTreeRoot(MMRData zRoot){
		mRoot = zRoot;
	}
	
	public MMRData getPublicKeyTreeRoot(){
		return mRoot;
	}
}
