package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;

public class Proof implements Streamable {

	public class ProofChunk {
		MiniHash mHash;
		MiniByte mLeftRight;
		public ProofChunk(MiniByte zLeft, MiniHash zHash) {
			mLeftRight = zLeft;
			mHash = zHash;
		}
		
		public MiniByte getLeft() {
			return mLeftRight;
		}
		
		public MiniHash getHash() {
			return mHash;
		}
	}
	
	//The data you are trying to prove..
	MiniHash mData;
	
	//The Merkle Branch that when applied to the data gives the final proof;
	ArrayList<ProofChunk> mProofChain;
	
	private Proof(){}
	
	public Proof(MiniHash zData) {
		mData   = zData;
		mProofChain = new ArrayList<>();
	}
	
	public void addProofChunk(MiniByte zLeft, MiniHash zHash) {
		mProofChain.add(new ProofChunk(zLeft, zHash));
	}
	
	public int getProofLen() {
		return mProofChain.size();
	}
	
	public MiniHash calculateFinalHash() {
		//Get the Final Hash of the Data
		MiniHash current = mData;
		
		int len = getProofLen();
		for(int i=0;i<len;i++) {
			ProofChunk chunk = mProofChain.get(i);
			
			if(chunk.getLeft().isTrue()) {
				current = Crypto.getInstance().hashObjects(chunk.getHash(), current);
			}else {
				current = Crypto.getInstance().hashObjects(current, chunk.getHash());
			}
		}
		
		return current;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mData.writeDataStream(zOut);
		
		int len = mProofChain.size();
		zOut.writeInt(len);
		for(int i=0;i<len;i++) {
			ProofChunk chunk = mProofChain.get(i);
			chunk.getLeft().writeDataStream(zOut);
			chunk.getHash().writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData = MiniHash.ReadFromStream(zIn);
		mProofChain = new ArrayList<>();
		int len = zIn.readInt();
		for(int i=0;i<len;i++) {
			MiniByte left = MiniByte.ReadFromStream(zIn);
			MiniHash hash = MiniHash.ReadFromStream(zIn);
			mProofChain.add(new ProofChunk(left, hash));
		}
	}
	
	public static Proof ReadFromStream(DataInputStream zIn){
		Proof proof = new Proof();
		
		try {
			proof.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return proof;
	}
}
