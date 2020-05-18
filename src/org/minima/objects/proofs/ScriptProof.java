package org.minima.objects.proofs;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONObject;

public class ScriptProof extends Proof {

	MiniString mScript;
	
	private ScriptProof() {
		super();
	}
	
	public ScriptProof(String zScript, int zBitLength) throws Exception {
		super();
		
		if(zBitLength == 512) {
			init(zScript,"0x10");
		}else if(zBitLength == 384) {
			init(zScript,"0x0C");
		}else if(zBitLength == 320) {
			init(zScript,"0x0A");
		}else if(zBitLength == 288) {
			init(zScript,"0x09");
		}else if(zBitLength == 256) {
			init(zScript,"0x08");
		}else if(zBitLength == 224) {
			init(zScript,"0x07");
		}else if(zBitLength == 192) {
			init(zScript,"0x06");
		}else if(zBitLength == 160) {
			init(zScript,"0x05");
		}else if(zBitLength == 128) {
			init(zScript,"0x04");
		}else {
			throw new Exception("Invalid Bitlength fro script proof "+zBitLength);
		}
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
