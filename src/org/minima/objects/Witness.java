package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniString;
import org.minima.objects.proofs.ScriptProof;
import org.minima.objects.proofs.SignatureProof;
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
	
//	/**
//	 * The Scripts for the Inputs
//	 */
//	ArrayList<String> mScripts;
//	
	/**
	 * The Signatures
	 */
	ArrayList<SignatureProof> mSignatureProofs;
	
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
		
//		mScripts    = new ArrayList<>();
		mProofs     = new ArrayList<>();
		
		mSignatureProofs = new ArrayList<>();
		
		//Token details..
		mTokenDetails = new ArrayList<>();
	}
	
	public void addSignature(MiniHash zPubKey, MiniData zSignature) {
		mSignatureProofs.add(new SignatureProof(zPubKey, zSignature));
	}
	
	public void addSignature(SignatureProof zSigProof) {
		mSignatureProofs.add(zSigProof);
	}
	
	public ArrayList<SignatureProof> getAllSignatures(){
		return mSignatureProofs;
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

	public String getAllPubKeysCSV(){
		String ret = "";
		for(SignatureProof sig : mSignatureProofs) {
			ret += sig.getFinalHash().to0xString()+"#";
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
		
		//Signatures
		JSONArray arr = new JSONArray();
		for(SignatureProof sg : mSignatureProofs) {
			arr.add(sg.toJSON());
		}
		obj.put("signatures", arr);

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
		//Signatures 
		int pklen = mSignatureProofs.size();
		zOut.writeInt(pklen);
		for(SignatureProof sp : mSignatureProofs) {
			//The Pub Key
			sp.writeDataStream(zOut);
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
		mSignatureProofs = new ArrayList<>();
		int prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mSignatureProofs.add(SignatureProof.ReadFromStream(zIn));
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
