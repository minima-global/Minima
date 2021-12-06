package org.minima.objects.keys;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class SignatureProof implements Streamable {

	private MiniData mPublicKey;
	
	private MiniData mSignature;
	
	private MMRProof mProof;
	
	private SignatureProof() {}

	public SignatureProof(MiniData zPublicKey, MiniData zSignature, MMRProof zProof) {
		mPublicKey 	= zPublicKey;
		mSignature 	= zSignature;
		mProof 		= zProof;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getSignature() {
		return mSignature;
	}
	
	public MMRProof getProof() {
		return mProof;
	}
	
	public MiniData getRootPublicKey(){
		return mProof.calculateProof(new MMRData(mPublicKey)).getData();
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("publickey", mPublicKey.to0xString());
		json.put("rootkey", getRootPublicKey().to0xString());
		json.put("proof", mProof.toJSON());
		json.put("signature", mSignature.to0xString());

		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
		mProof.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey	= MiniData.ReadFromStream(zIn);
		mSignature 	= MiniData.ReadFromStream(zIn);
		mProof		= MMRProof.ReadFromStream(zIn);
	}
	
	public static SignatureProof ReadFromStream(DataInputStream zIn) throws IOException {
		SignatureProof sig = new SignatureProof();
		sig.readDataStream(zIn);
		return sig;
	}
}
