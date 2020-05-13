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

public class MultiSig implements Streamable {

	/**
	 * Each level of any key tree has this
	 */
	MiniData mPublicKey;
	MiniData mProofChain;
	MiniData mSignature;
	
	/**
	 * The parent tree - can be null if this is the lowest level
	 */
	MiniData mParentSignature;
	
	public MultiSig(MiniData zPubKey, MiniData zProofChain, MiniData zSignature) {
		this(zPubKey, zProofChain, zSignature, null);
	}
	
	public MultiSig(MiniData zPubKey, MiniData zProofChain, MiniData zSignature, MiniData zParentSignature) {
		mPublicKey       = zPubKey;
		mProofChain      = zProofChain;
		mSignature       = zSignature;
		
		//The Parent Multi Sig
		mParentSignature = zParentSignature;
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
	
	public boolean hasParentSignature() {
		return mParentSignature != null;
	}
	
	public MiniData getParentSignature() {
		return mParentSignature;
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
	
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPublicKey.writeDataStream(zOut);
		mProofChain.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
		
		if(mParentSignature==null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mParentSignature.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey  = MiniData.ReadFromStream(zIn);
		mProofChain = MiniData.ReadFromStream(zIn);
		mSignature  = MiniData.ReadFromStream(zIn);
	
		//Is there a parent..
		mParentSignature = null;
		if(MiniByte.ReadFromStream(zIn).isTrue()) {
			mParentSignature = MiniData.ReadFromStream(zIn);
		}
	}
}
