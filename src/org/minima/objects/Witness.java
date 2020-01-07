package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniKeyValue;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.utils.Crypto;
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
	
	
	
	//--------------NEW WAY ----------------//
	
	/**
	 * An array of the public keys and the corresponding signatures
	 */
//	ArrayList<KeyValueMiniData> mPubkSigs;
	
	
	ArrayList<MiniKeyValue> mAllScripts;
	
	
	ArrayList<MiniKeyValue> mTokenProof;
	
	
	
	
	
	/**
	 * General Constructor
	 */
	public Witness() {
//		mPubkSigs = new ArrayList<>();
		
		mPublicKeys = new ArrayList<>();
		
		mSignatures = new ArrayList<>();
		
		mScripts    = new ArrayList<>();
		mProofs     = new ArrayList<>();
		
		mAllScripts = new ArrayList<MiniKeyValue>();
		
		mTokenProof = new ArrayList<MiniKeyValue>();
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
	
	public ArrayList<MMRProof> getAllProofs(){
		return mProofs;
	}
	
	public void addSignature(MiniData zPublicKey, MiniData zSig) {
		//Add the public Key
		mPublicKeys.add(zPublicKey);
		
		//Add to signatures
		mSignatures.add(zSig);
		
//		KeyValueMiniData pubk = new KeyValueMiniData(zPublicKey, zSig);
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
	
//	public void setTokenProof(MiniData32 zTokenID, MiniData zTokenProof) {
//		mTokenProof.add(new MiniKeyValue(zTokenID, zTokenProof));
//	}
//	
//	public MiniData getTokenProof(MiniData32 zTokenID) {
//		for(Mini)  
//	}
	
	
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
	}
}
