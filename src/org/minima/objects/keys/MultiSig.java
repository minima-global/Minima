package org.minima.objects.keys;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.proofs.Proof;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MultiSig implements Streamable {

	/**
	 * Each level of any key tree has this
	 */
	MiniData mPublicKey;
	MiniData mProofChain;
	MiniData mSignature;
	
	/**
	 * The Child tree - can be null if this is the top level
	 */
	MiniData mChildSignature;
	
	public MultiSig(MiniData zPubKey, MiniData zProofChain, MiniData zSignature) {
		this(zPubKey, zProofChain, zSignature, null);
	}
	
	public MultiSig(MiniData zPubKey, MiniData zProofChain, MiniData zSignature, MiniData zChildSignature) {
		mPublicKey       = zPubKey;
		mProofChain      = zProofChain;
		mSignature       = zSignature;
		
		//The Parent Multi Sig
		mChildSignature = zChildSignature;
	}
	
	public MultiSig(MiniData zCompleteSignature) {
		//Write it out..
		try {
			ByteArrayInputStream bais = new ByteArrayInputStream(zCompleteSignature.getData());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now read the data
			readDataStream(dis);
			
			dis.close();
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getSignature() {
		return mSignature;
	}
	
	public MiniData getProofChain() {
		return mProofChain;
	}
	
	public boolean hasChildSignature() {
		return mChildSignature != null;
	}
	
	public MiniData getChildSignature() {
		return mChildSignature;
	}
	
	public MiniData getRootKey() {
		Proof chainproof = new Proof();

		//Hash the Input..		
		chainproof.setData(mPublicKey);
		chainproof.setProof(mProofChain);
		
		return chainproof.getFinalHash();
	}
	
	public MiniData getCompleteSig() {
		//Write it out..
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			
			//Now write the data
			writeDataStream(dos);
			
			dos.flush();
			dos.close();
			
			return new MiniData(baos.toByteArray());
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
		
		return null;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("publickey", mPublicKey.to0xString());
		json.put("proof", mProofChain.to0xString());
		json.put("signature", mSignature.to0xString());
		if(mChildSignature == null) {
			json.put("childsig", "0x00");
		}else {
			json.put("childsig", mChildSignature.to0xString());
		}
		
		return json;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mProofChain.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
		
		if(mChildSignature==null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mChildSignature.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey  = MiniData.ReadFromStream(zIn);
		mProofChain = MiniData.ReadFromStream(zIn);
		mSignature  = MiniData.ReadFromStream(zIn);
	
		//Is there a parent..
		mChildSignature = null;
		if(MiniByte.ReadFromStream(zIn).isTrue()) {
			mChildSignature = MiniData.ReadFromStream(zIn);
		}
	}
}
