package org.minima.objects.proofs;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.BaseConverter;
import org.minima.utils.json.JSONObject;

public class ScriptProof extends Proof {

	MiniString mScript;
	
	private ScriptProof() {
		super();
	}
	
	public ScriptProof(String zScript, int zBitLength) throws Exception {
		super();
		init(zScript,BaseConverter.numberToHex(zBitLength/32));
	}
	
	public ScriptProof(String zScript, String zChainSHAProof) throws Exception {
		super();
		init(zScript,zChainSHAProof);
	}
	
	private void init(String zScript, String zChainSHAProof) throws Exception {
		mScript = new MiniString(zScript);
		
		//How many Bits in HASH
		int bits = Proof.getChainSHABits(zChainSHAProof);
		
		//Create an address
		Address addr = new Address(mScript.toString(),bits);
		setData(addr.getAddressData());
		setHashBitLength(bits);
		
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
	
	public static ScriptProof ReadFromStream(DataInputStream zIn) throws IOException{
		ScriptProof proof = new ScriptProof();
		proof.readDataStream(zIn);
		return proof;
	}
}
