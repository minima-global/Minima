package org.minima.utils.sphincs;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.json.JSONObject;

public class Signature {

	/**
	 * These are the Private key values at the referenced position in the private key
	 * 
	 * The Pre-images of the Public key values..
	 */
	ArrayList<MiniData> mSigValues = new ArrayList<>();
	
	/**
	 * These are the MMRProofs of each public key value in the public key tree
	 */
	ArrayList<MMRProof>  mPublicKeyProofs = new ArrayList<>();
	
	public Signature() {}
	
	public ArrayList<MiniData> getSignatureValues(){
		return mSigValues;
	}
	
	public ArrayList<MMRProof> getPublicKeyTreeProofs(){
		return mPublicKeyProofs;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		int sigsize = mSigValues.size();
		for(int i=0;i<sigsize;i++) {
			//json.put(""+i, mSigValues[i]);
		}
		
		
		return json;
	}
}
