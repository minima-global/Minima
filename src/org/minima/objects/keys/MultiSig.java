package org.minima.objects.keys;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.proofs.Proof;
import org.minima.utils.Streamable;

public class MultiSig implements Streamable {

	MiniData mPublicKey;
	
	MiniData mProofChain;
	
	MiniData mSignature;
	
	public MultiSig(MiniData zPubKey, MiniData zProofChain, MiniData zSignature) {
		mPublicKey  = zPubKey;
		mProofChain = zProofChain;
		mSignature  = zSignature;
	}
	
	public MultiSig(MiniData zCompleteSig) {
		//Write it out..
		try {
			ByteArrayInputStream bais = new ByteArrayInputStream(zCompleteSig.getData());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now read the data
			readDataStream(dis);
			
			dis.close();
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
	}
	
	public MiniData getPubKey() {
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
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPublicKey  = MiniData.ReadFromStream(zIn);
		mProofChain = MiniData.ReadFromStream(zIn);
		mSignature  = MiniData.ReadFromStream(zIn);
	}
}
