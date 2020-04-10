package org.minima.objects.proofs;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.kissvm.Contract;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniScript;
import org.minima.utils.json.JSONObject;

public class ScriptProof extends Proof {

	MiniScript mScript;
	
	private ScriptProof() {
		super();
	}
	
	/**
	 * Create a simple one hash Proof for a script
	 * @param zScript
	 * @throws Exception 
	 */
//	public ScriptProof(String zScript) throws Exception {
//		super();
//		init(zScript,"0x0200");
//	}
	
	public ScriptProof(String zScript, int zBitLength) throws Exception {
		super();
		
		if(zBitLength == 512) {
			init(zScript,"0x0200");
		}else if(zBitLength == 256) {
			init(zScript,"0x0100");
		}else if(zBitLength == 160) {
			init(zScript,"0x00A0");
		} 
	}
	
	public ScriptProof(String zScript, String zChainSHAProof) throws Exception {
		super();
		init(zScript,zChainSHAProof);
	}
	
	private void init(String zScript, String zChainSHAProof) throws Exception {
		mScript = new MiniScript(zScript);
		
		//How many Bits in HASH
		int bits = Proof.getChainSHABits(zChainSHAProof);
		
		//Create an address
		Address addr = new Address(mScript.toString(),bits);
		setData(addr.getAddressData());
		setHashBitLength(bits);
		
		setProof(new MiniData(zChainSHAProof));
		
		finalizeHash();
	}
	
	
	public MiniScript getScript() {
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
		mScript = MiniScript.ReadFromStream(zIn);
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
