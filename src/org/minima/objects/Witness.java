package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Witness implements Streamable {
	
	/**
	 * All the signatures required for all the Inputs to succeed. 
	 * You sign the TXN_ID. 
	 */
	ArrayList<MiniData> mPublicKeys;
	
	/**
	 * The Signatures - These are a LARGE part of the data. And ONLY checked once on entry to the system. 
	 */
	ArrayList<MiniData> mSignatures;
	
	/**
	 * The MMR Proofs that each input Coin is valid and unspent.
	 */
	ArrayList<MMRProof> mProofs;
	
	/**
	 * The Scripts for the Inputs
	 */
	ArrayList<String> mScripts;
	
	/**
	 * Token generation details.. one per transaction
	 */
	TokenDetails mTokenGenDetails = null;
	
	/**
	 * Any tokens used in any inputs must provide the Token Details
	 */
	ArrayList<TokenDetails> mTokenDetails;
	
	/**
	 * General Constructor
	 */
	public Witness() {
		mPublicKeys = new ArrayList<>();
		mSignatures = new ArrayList<>();
		
		mScripts    = new ArrayList<>();
		mProofs     = new ArrayList<>();
		
		//Token details..
		mTokenDetails = new ArrayList<>();
	}
	
	public void addScript(String zScript) {
		mScripts.add(zScript);
	}
	
	public String getScript(int zScript) {
		return mScripts.get(zScript);
	}
	
	public void addMMRProof(MMRProof zProof) {
		mProofs.add(zProof);
	}
	
	public void clearProofs(){
		mProofs.clear();	
	}
	
	public ArrayList<MMRProof> getAllProofs(){
		return mProofs;
	}
	
	public void addSignature(MiniData zPublicKey, MiniData zSig) {
		//Add the public Key
		mPublicKeys.add(zPublicKey);
		
		//Add to signatures
		mSignatures.add(zSig);
	}
	
	public MiniData getPublicKey(int zPubk) {
		return mPublicKeys.get(zPubk);
	}
	
	public MiniData getSignature(int zSig) {
		return mSignatures.get(zSig);
	}
	
	public ArrayList<MiniData> getAllSignatures(){
		return mSignatures;
	}
	
	public ArrayList<MiniData> getAllPubKeys(){
		return mPublicKeys;
	}
	
	public String getAllPubKeysCSV(){
		String ret = "";
		for(MiniData sig : mPublicKeys) {
			ret += sig.toString()+",";
		}
		return ret.trim();
	}
	
	public void setTokenGenDetails(TokenDetails zTokGenDetails) {
		mTokenGenDetails = zTokGenDetails;
	}
	
	public TokenDetails getTokenGenDetails() {
		return mTokenGenDetails;
	}
	
	public ArrayList<TokenDetails> getAllTokenDetails(){
		return mTokenDetails;
	}
	
	public void addTokenDetails(TokenDetails zDetails) {
		mTokenDetails.add(zDetails);
	}
	
	public TokenDetails getTokenDetail(MiniHash zTokenID) {
		for(TokenDetails td : mTokenDetails) {
			if(td.getTokenID().isExactlyEqual(zTokenID)) {
				return td;
			}
		}
		
		return null;
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		//Pub Keys
		JSONArray arr = new JSONArray();
		for(MiniData pubk : mPublicKeys) {
			arr.add(pubk.toString());
		}
		obj.put("publickeys", arr);
		
		//Signatures
		arr = new JSONArray();
		for(MiniData sig : mSignatures) {
			arr.add(sig.toString());
		}
		obj.put("signatures", arr);
		
		//Scripts
		arr = new JSONArray();
		for(String script : mScripts) {
			arr.add(script);
		}
		obj.put("scripts", arr);
		
		//MMRProofs
		arr = new JSONArray();
		for(MMRProof proof : mProofs) {
			arr.add(proof.toJSON());
		}
		obj.put("mmrproofs", arr);

		//Token Details
		arr = new JSONArray();
		for(TokenDetails td : mTokenDetails) {
			arr.add(td.toJSON());
		}
		obj.put("tokens", arr);
				
		//Token Generation..
		if(mTokenGenDetails != null) {
			obj.put("tokengen", mTokenGenDetails.toJSON());
		}
		
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Pub Keys
		int pklen = mPublicKeys.size();
		zOut.writeInt(pklen);
		for(MiniData pubk : mPublicKeys) {
			//The Pub Key
			pubk.writeDataStream(zOut);
		}
		
		//Signatures
		zOut.writeInt(mSignatures.size());
		for(MiniData sig : mSignatures) {
			//The Pub Key
			sig.writeDataStream(zOut);
		}
		
		//Scripts
		int sclen = mScripts.size();
		zOut.writeInt(sclen);
		for(String script : mScripts) {
			zOut.writeUTF(script);
		}
		
		//MMRProofs
		int mmrlen = mProofs.size();
		zOut.writeInt(mmrlen);
		for(MMRProof proof : mProofs) {
			proof.writeDataStream(zOut);
		}
		
		//Token Details
		int toklen = mTokenDetails.size();
		zOut.writeInt(toklen);
		for(TokenDetails td : mTokenDetails) {
			td.writeDataStream(zOut);
		}
		
		//Token generation
		if(mTokenGenDetails == null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mTokenGenDetails.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKeys = new ArrayList<>();
		int prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mPublicKeys.add(MiniData.ReadFromStream(zIn));
		}
	
		//Read in the combined sig hash
		mSignatures = new ArrayList<>();
		int siglen = zIn.readInt();
		for(int i=0;i<siglen;i++) {
			mSignatures.add(MiniData.ReadFromStream(zIn));
		}
		
		mScripts = new ArrayList<>();
		prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mScripts.add(zIn.readUTF());
		}
		
		mProofs = new ArrayList<>();
		prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mProofs.add(MMRProof.ReadFromStream(zIn));
		}
		
		mTokenDetails = new ArrayList<>();
		prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mTokenDetails.add(TokenDetails.ReadFromStream(zIn));
		}
		
		//Token generation
		MiniByte tokgen = MiniByte.ReadFromStream(zIn);
		if(tokgen.isTrue()) {
			mTokenGenDetails = TokenDetails.ReadFromStream(zIn);
		}else {
			mTokenGenDetails = null;
		}
	}
}
