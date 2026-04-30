package org.minima.utils.sphincs.HORST;

import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class HORSTSignature {

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
	
	public HORSTSignature() {}
	
	public ArrayList<MiniData> getSignatureValues(){
		return mSigValues;
	}
	
	public ArrayList<MMRProof> getPublicKeyTreeProofs(){
		return mPublicKeyProofs;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		int sigsize = mSigValues.size();
		json.put("size", sigsize);
		
		for(int i=0;i<sigsize;i++) {
			
			JSONObject sigval = new JSONObject();
			sigval.put("SigValue", mSigValues.get(i).to0xString());
			
			MiniData proofdata = MiniData.getMiniDataVersion(mPublicKeyProofs.get(i));
			sigval.put("SigProof", proofdata.to0xString());
			
			json.put("Chunk_"+i, sigval);
		}
		
		return json;
	}
}
