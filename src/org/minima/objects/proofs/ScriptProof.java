package org.minima.objects.proofs;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.objects.Address;
import org.minima.objects.Proof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class ScriptProof implements Streamable {

	MiniString mScript;
	Proof mProof;
	
	private ScriptProof() {}
	
	/**
	 * Create a simple one hash Proof for a script
	 * @param zScript
	 */
	public ScriptProof(String zScript) {
		mScript = new MiniString(Contract.cleanScript(zScript));
		
		//Create an address
		Address addr = new Address(mScript.toString());
		
		//Now create the proof..
		mProof  = new Proof(addr.getAddressData());
		mProof.finalizeHash();
	}
	
	public ScriptProof(String zScript, String zChainSHAProof) {
		mScript = new MiniString(Contract.cleanScript(zScript));
		
		//Create an address
		Address addr = new Address(mScript.toString());
				
		mProof  = new Proof(addr.getAddressData(), new MiniData(zChainSHAProof));
	}
	
	public ScriptProof(String zScript, Proof zProof) {
		mScript = new MiniString(Contract.cleanScript(zScript));
		mProof  = zProof;
	}
	
	public MiniString getScript() {
		return mScript;
	}
	
	public Proof getProof() {
		return mProof;
	}
	
	public MiniHash getProofHash() {
		return mProof.calculateFinalHash();
	}

	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("script", mScript.toString());
		json.put("proof", mProof.toJSON());
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mScript.writeDataStream(zOut);
		mProof.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mScript = MiniString.ReadFromStream(zIn);
		mProof  = Proof.ReadFromStream(zIn);
	}
	
	public static ScriptProof ReadFromStream(DataInputStream zIn){
		ScriptProof proof = new ScriptProof();
		
		try {
			proof.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return proof;
	}
}
