package org.minima.objects.proofs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

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
	protected MiniHash mData;
	
	//The Merkle Branch that when applied to the data gives the final proof;
	protected ArrayList<ProofChunk> mProofChain;
	
	//Calculate this once
	protected MiniHash mFinalHash;
	protected MiniData mChainSHA;
	protected boolean mFinalized;
		
	public Proof(){
		mProofChain = new ArrayList<>();
	}

	public void setData(MiniHash zData) {
		mData = zData;
	}
	
	public MiniHash getData() {
		return mData;
	}
	
	public void setProof(MiniData zChainSHAProof) {
		mFinalized  = false;
		mProofChain = new ArrayList<>();
		
		byte[] chdata = zChainSHAProof.getData();
		
		ByteArrayInputStream bais = new ByteArrayInputStream(chdata);
		DataInputStream dis = new DataInputStream(bais);
		
		int len  = chdata.length;  
		int read = 0;
		while(read<len) {
			//Is it to the left or the right 
			MiniByte leftrigt = MiniByte.ReadFromStream(dis);
			read++;
			
			//What data to hash
			MiniHash data = MiniHash.ReadFromStream(dis);
			read += data.getLength();
			
			//Add to the Proof..
			addProofChunk(leftrigt, data);
		}
		
		finalizeHash();
	}
	
	public void addProofChunk(MiniByte zLeft, MiniHash zHash) {
		mProofChain.add(new ProofChunk(zLeft, zHash));
	}
	
	public int getProofLen() {
		return mProofChain.size();
	}
	
	public ProofChunk getProofChunk(int zNum) {
		return mProofChain.get(zNum);
	}
	
	public void finalizeHash() {
		//Reset so that can be recalculated
		mFinalized = false;
		
		//Recalculate
		mFinalHash = getFinalHash();
		mChainSHA  = getChainSHAProof();
		
		//Ok - it's done now..
		mFinalized = true;
	}
	
	public MiniData getChainSHAProof() {
		if(mFinalized) {
			return mChainSHA;
		}
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		
		try {
			int len = mProofChain.size();
			for(int i=0;i<len;i++){
				ProofChunk chunk = mProofChain.get(i);
				chunk.getLeft().writeDataStream(dos);
				chunk.getHash().writeDataStream(dos);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		//Convert to MiniData..
		return new MiniData(baos.toByteArray());
	}
	
	public MiniHash getFinalHash() {
		if(mFinalized) {
			return mFinalHash;
		}
		
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

	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		JSONArray proof = new JSONArray();
		int len = mProofChain.size();
		for(int i=0;i<len;i++){
			JSONObject jsonchunk = new JSONObject();
			
			ProofChunk chunk = mProofChain.get(i);
			jsonchunk.put("left", chunk.getLeft().isTrue());
			jsonchunk.put("hash", chunk.getHash().to0xString());
			proof.add(jsonchunk);
		}
		
		json.put("data", mData.to0xString());
		json.put("proofchain", proof);
		json.put("chainsha", getChainSHAProof().to0xString());
		json.put("finalhash", getFinalHash().to0xString());
		
		return json;
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
		
		finalizeHash();
	}
	
	public static Proof ReadFromStream(DataInputStream zIn){
		Proof proof = new Proof();
		
		try {
			proof.readDataStream(zIn);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return proof;
	}
}
