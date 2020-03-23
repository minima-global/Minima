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
	 * The MMR Proofs that each input Coin is valid and unspent.
	 */
	ArrayList<MMRProof> mMMRProofs;
	
	/**
	 * The Signatures
	 */
	ArrayList<SignatureProof> mSignatureProofs;
	
	/**
	 * Any tokens used in any inputs must provide the Token Details
	 */
	ArrayList<TokenDetails> mTokenDetails;
	
	/**
	 * General Constructor
	 */
	public Witness() {
		mMMRProofs     = new ArrayList<>();
		mSignatureProofs = new ArrayList<>();
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
		mMMRProofs.add(zProof);
	}
	
	public void clearProofs(){
		mMMRProofs.clear();	
	}
	
	public ArrayList<MMRProof> getAllProofs(){
		return mMMRProofs;
	}

	public String getAllPubKeysCSV(){
		String ret = "";
		for(SignatureProof sig : mSignatureProofs) {
			ret += sig.getFinalHash().to0xString()+"#";
		}

		return ret.trim();
	}
	
	public ArrayList<TokenDetails> getAllTokenDetails(){
		return mTokenDetails;
	}
	
	public void addTokenDetails(TokenDetails zDetails) {
		if(getTokenDetail(zDetails.getTokenID()) == null){
			mTokenDetails.add(zDetails);	
		}
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
		for(MMRProof proof : mMMRProofs) {
			arr.add(proof.toJSON());
		}
		obj.put("mmrproofs", arr);

		//Token Details
		arr = new JSONArray();
		for(TokenDetails td : mTokenDetails) {
			arr.add(td.toJSON());
		}
		obj.put("tokens", arr);
		
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
		int mmrlen = mMMRProofs.size();
		zOut.writeInt(mmrlen);
		for(MMRProof proof : mMMRProofs) {
			proof.writeDataStream(zOut);
		}
		
		//Token Details
		int toklen = mTokenDetails.size();
		zOut.writeInt(toklen);
		for(TokenDetails td : mTokenDetails) {
			td.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mSignatureProofs = new ArrayList<>();
		int prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mSignatureProofs.add(SignatureProof.ReadFromStream(zIn));
		}
		
		mMMRProofs = new ArrayList<>();
		prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mMMRProofs.add(MMRProof.ReadFromStream(zIn));
		}
		
		mTokenDetails = new ArrayList<>();
		prlen = zIn.readInt();
		for(int i=0;i<prlen;i++) {
			mTokenDetails.add(TokenDetails.ReadFromStream(zIn));
		}
	}
}
