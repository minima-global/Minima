package org.minima.objects.proofs;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONObject;

public class ScriptProof extends Proof {

	MiniString mScript;
	
	private ScriptProof() {
		super();
	}
	
	/**
	 * Create a simple one hash Proof for a script
	 * @param zScript
	 */
	public ScriptProof(String zScript) {
		this(zScript,"");
	}
	
	public ScriptProof(String zScript, String zChainSHAProof) {
		mScript = new MiniString(Contract.cleanScript(zScript));
		
		//Create an address
		Address addr = new Address(mScript.toString());
		
		setData(addr.getAddressData());
		setProof(new MiniData(zChainSHAProof));
		
		finalizeHash();
	}
	
	public MiniString getScript() {
		return mScript;
	}
	
	@Override
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("script", mScript.toString());
		json.put("proof", super.toJSON());
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mScript.writeDataStream(zOut);
		super.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mScript = MiniString.ReadFromStream(zIn);
		super.readDataStream(zIn);
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
