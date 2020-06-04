package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.ScriptProof;
import org.minima.objects.proofs.SignatureProof;
import org.minima.objects.proofs.TokenProof;
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
	ArrayList<TokenProof> mTokenProofs;
	
	/**
	 * The Scripts used in the transactions 
	 * 
	 * Addresses
	 * Tokens
	 * MAST
	 */
	protected ArrayList<ScriptProof> mScriptProofs;
	
	/**
	 * General Constructor
	 */
	public Witness() {
		mMMRProofs       = new ArrayList<>();
		mSignatureProofs = new ArrayList<>();
		mTokenProofs     = new ArrayList<>();
		mScriptProofs    = new ArrayList<>();
	}
	
	/**
	 * Signature functions
	 */
	public void addSignature(MiniData zPubKey, MiniData zSignature) {
		mSignatureProofs.add(new SignatureProof(zPubKey, zSignature));
	}
	
	public void addSignature(SignatureProof zSigProof) {
		mSignatureProofs.add(zSigProof);
	}
	
	public ArrayList<SignatureProof> getAllSignatures(){
		return mSignatureProofs;
	}
	
	public void clearSignatures() {
		mSignatureProofs.clear();
	}
	
	public String getAllPubKeysCSV(){
		String ret = "";
		for(SignatureProof sig : mSignatureProofs) {
			ret += sig.getFinalHash().to0xString()+" # ";
		}

		return ret.trim();
	}
	
	/**
	 * MMR Functions
	 */
	public void addMMRProof(MMRProof zProof) {
		mMMRProofs.add(zProof);
	}
	
	public void clearMMRProofs(){
		mMMRProofs.clear();	
	}
	
	public ArrayList<MMRProof> getAllMMRProofs(){
		return mMMRProofs;
	}

	
	/**
	 * Token Proofs
	 */
	
	public ArrayList<TokenProof> getAllTokenDetails(){
		return mTokenProofs;
	}
	
	public void addTokenDetails(TokenProof zDetails) {
		if(getTokenDetail(zDetails.getTokenID()) == null){
			mTokenProofs.add(zDetails);	
		}
	}
	
	public TokenProof getTokenDetail(MiniData zTokenID) {
		for(TokenProof td : mTokenProofs) {
			if(td.getTokenID().isEqual(zTokenID)) {
				return td;
			}
		}
		return null;
	}
	
	/**
	 * Script Proofs
	 */
	public boolean addScript(String zScript, int zBitLength) throws Exception {
		return addScript(new ScriptProof(zScript,zBitLength));
	}
	
	public boolean addScript(ScriptProof zScriptProof) {
		if(!scriptExists(zScriptProof.getFinalHash())) {
			mScriptProofs.add(zScriptProof);		
			return true;
		}
		return false;
	}
	
	public ScriptProof getScript(MiniData zAddress) {
		for(ScriptProof proof : mScriptProofs) {
			if(proof.getFinalHash().isEqual(zAddress)) {
				return proof;
			}
		}
		
		return null;
	}
	
	public boolean scriptExists(MiniData zHash) {
		return getScript(zHash)!=null;
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
		for(TokenProof td : mTokenProofs) {
			arr.add(td.toJSON());
		}
		obj.put("tokens", arr);
		
		//Scripts
		arr = new JSONArray();
		for(ScriptProof sp : mScriptProofs) {
			arr.add(sp.toJSON());
		}
		obj.put("scripts", arr);
		
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Signatures 
		MiniNumber len = new MiniNumber(mSignatureProofs.size());
		len.writeDataStream(zOut);
		for(SignatureProof sp : mSignatureProofs) {
			sp.writeDataStream(zOut);
		}
		
		//MMRProofs
		len = new MiniNumber(mMMRProofs.size());
		len.writeDataStream(zOut);
		for(MMRProof proof : mMMRProofs) {
			proof.writeDataStream(zOut);
		}
		
		//Tokens
		len = new MiniNumber(mTokenProofs.size());
		len.writeDataStream(zOut);
		for(TokenProof td : mTokenProofs) {
			td.writeDataStream(zOut);
		}
		
		//Scripts
		len = new MiniNumber(mScriptProofs.size());
		len.writeDataStream(zOut);
		for(ScriptProof sp : mScriptProofs) {
			sp.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mSignatureProofs = new ArrayList<>();
		MiniNumber mlen = MiniNumber.ReadFromStream(zIn);
		int len = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mSignatureProofs.add(SignatureProof.ReadFromStream(zIn));
		}
		
		mMMRProofs = new ArrayList<>();
		mlen = MiniNumber.ReadFromStream(zIn);
		len  = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mMMRProofs.add(MMRProof.ReadFromStream(zIn));
		}
		
		mTokenProofs = new ArrayList<>();
		mlen = MiniNumber.ReadFromStream(zIn);
		len  = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mTokenProofs.add(TokenProof.ReadFromStream(zIn));
		}
		
		mScriptProofs = new ArrayList<>();
		mlen = MiniNumber.ReadFromStream(zIn);
		len  = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mScriptProofs.add(ScriptProof.ReadFromStream(zIn));
		}
	}
}
