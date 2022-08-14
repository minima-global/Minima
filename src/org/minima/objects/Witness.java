package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Witness implements Streamable {
	
	/**
	 * The Signatures
	 */
	private ArrayList<Signature> mSignatureProofs;
	
	/**
	 * The MMR Proofs that each input Coin is valid and unspent.
	 */
	private ArrayList<CoinProof> mCoinProofs;
	
	/**
	 * The Scripts used in the transactions 
	 */
	private ArrayList<ScriptProof> mScriptProofs;

	/**
	 * General Constructor
	 */
	public Witness() {
		mCoinProofs       	= new ArrayList<>();
		mSignatureProofs 	= new ArrayList<>();
		mScriptProofs    	= new ArrayList<>();
	}
	
	/**
	 * Signature functions
	 */
	public void addSignature(Signature zSigProof) {
		if(zSigProof == null) {
			throw new IllegalArgumentException("Cannot add a NULL Signature");
		}
		
		//Check not already added
		if(!isSignedBy(zSigProof.getRootPublicKey().to0xString())) {
			mSignatureProofs.add(zSigProof);
		}
	}
	
	public ArrayList<Signature> getAllSignatures(){
		return mSignatureProofs;
	}
	
	public ArrayList<MiniData> getAllSignatureKeys(){
		ArrayList<MiniData> pubkeys = new ArrayList<>();
		for(Signature sigproof : mSignatureProofs) {
			
			//Get the root key
			pubkeys.add(sigproof.getRootPublicKey());
		}
		return pubkeys;
	}
	
	public boolean isSignedBy(String zPublicKey) {
		ArrayList<MiniData> allkeys = getAllSignatureKeys();
		for(MiniData key : allkeys) {
			if(key.to0xString().equals(zPublicKey)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * MMR Functions
	 */
	public void clearCoinProofs() {
		mCoinProofs.clear();
	}
	
	public void addCoinProof(CoinProof zProof) {
		mCoinProofs.add(zProof);
	}
	
	public ArrayList<CoinProof> getAllCoinProofs(){
		return mCoinProofs;
	}

	/**
	 * Script Proofs
	 * 
	 * Scripts and their merkle paths to addresses or MAST used in the transaction
	 * 
	 */
	public void addScript(ScriptProof zScriptProof) {
		if(!scriptExists(zScriptProof.getAddress().getAddressData())) {
			mScriptProofs.add(zScriptProof);
		}
	}
	
	public ScriptProof getScript(MiniData zAddress) {
		for(ScriptProof proofscr : mScriptProofs) {
			if(proofscr.getAddress().getAddressData().isEqual(zAddress)) {
				return proofscr;
			}
		}
		
		return null;
	}
	
	private boolean scriptExists(MiniData zAddress) {
		return getScript(zAddress) != null;
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		//Signatures
		JSONArray arr = new JSONArray();
		for(Signature sg : mSignatureProofs) {
			arr.add(sg.toJSON());
		}
		obj.put("signatures", arr);

		//MMRProofs
		arr = new JSONArray();
		for(CoinProof proof : mCoinProofs) {
			arr.add(proof.toJSON());
		}
		obj.put("mmrproofs", arr);

		//Scripts
		arr = new JSONArray();
		for(ScriptProof proofscr : mScriptProofs) {
			arr.add(proofscr.toJSON());
		}
		obj.put("scripts", arr);

		//Tokens..
		//..
		
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Signatures 
		MiniNumber.WriteToStream(zOut, mSignatureProofs.size());
		for(Signature sp : mSignatureProofs) {
			sp.writeDataStream(zOut);
		}
		
		//MMRProofs
		MiniNumber.WriteToStream(zOut, mCoinProofs.size());
		for(CoinProof cproof : mCoinProofs) {
			cproof.writeDataStream(zOut);
		}
		
		//Scripts
		MiniNumber.WriteToStream(zOut, mScriptProofs.size());
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
			mSignatureProofs.add(Signature.ReadFromStream(zIn));
		}
		
		mCoinProofs = new ArrayList<>();
		mlen = MiniNumber.ReadFromStream(zIn);
		len  = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mCoinProofs.add(CoinProof.ReadFromStream(zIn));
		}
		
		mScriptProofs = new ArrayList<>();
		mlen = MiniNumber.ReadFromStream(zIn);
		len  = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			mScriptProofs.add(ScriptProof.ReadFromStream(zIn));
		}
	}
}
