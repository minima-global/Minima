package org.minima.objects.proofs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Proof implements Streamable {

	public class ProofChunk {
		MiniData mHash;
		MMRSumNumber mValue;
		MiniByte mLeftRight;
		
		public ProofChunk(MiniByte zLeft, MiniData zHash, MMRSumNumber zValue) {
			mLeftRight = zLeft;
			mHash = zHash;
			mValue = zValue;
		}
		
		public MiniByte getLeft() {
			return mLeftRight;
		}
		
		public MiniData getHash() {
			return mHash;
		}
		
		public MMRSumNumber getValue() {
			return mValue;
		}
	}
	
	//The data you are trying to prove..
	protected MiniData mData;
	
	//The Merkle Branch that when applied to the data gives the final proof;
	protected ArrayList<ProofChunk> mProofChain;
	
	//Calculate this once
	protected MiniData mFinalHash;
	protected MiniData mChainSHA;
	protected boolean mFinalized;
		
	private int HASH_BITS = 512;
	
	public Proof(){
		mProofChain = new ArrayList<>();
	}

	public void setData(MiniData zData) {
		mData = zData;
	}
	
	public MiniData getData() {
		return mData;
	}
	
	public void setProof(MiniData zChainSHAProof) {
		mFinalized  = false;
		mProofChain = new ArrayList<>();
	
		ByteArrayInputStream bais = new ByteArrayInputStream(zChainSHAProof.getData());
		DataInputStream dis = new DataInputStream(bais);
		
		try {
			//The HASH_BITS is first
			int hb    = MiniByte.ReadFromStream(dis).getValue();
			HASH_BITS = hb * 32;
			
			while(dis.available()>0) {
				//Is it to the left or the right 
				MiniByte leftrigt = MiniByte.ReadFromStream(dis);
				
				//What data to hash
				MiniData data = MiniData.ReadFromStream(dis);
				
				//Add to the Proof..
				addProofChunk(leftrigt, data);
			}
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log("setProof Error "+e);
			e.printStackTrace();
		}
		
		
		finalizeHash();
	}
	
	public void setHashBitLength(int zBitLength) {
		HASH_BITS = zBitLength;
	}
	
	public void addProofChunk(MiniByte zLeft, MiniData zHash) {
		addProofChunk(zLeft, zHash, MMRSumNumber.ZERO);
	}
	
	public void addProofChunk(MiniByte zLeft, MiniData zHash, MMRSumNumber zValue) {
		mProofChain.add(new ProofChunk(zLeft, zHash, zValue));
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
			//First write out the HASH_BITS
			MiniByte hb = new MiniByte(HASH_BITS / 32);
			hb.writeDataStream(dos);
			
			//Now write out the data..
			int len = mProofChain.size();
			for(int i=0;i<len;i++){
				ProofChunk chunk = mProofChain.get(i);
				chunk.getLeft().writeDataStream(dos);
				chunk.getHash().writeHashToStream(dos);
			}
			
			dos.close();
			baos.close();
			
		} catch (IOException e) {
			MinimaLogger.log("getChainSHAProof Error "+e);
			e.printStackTrace();
		}
		
		//Convert to MiniData..
		return new MiniData(baos.toByteArray());
	}
	
	public MiniData getFinalHash() {
		if(mFinalized) {
			return mFinalHash;
		}
		
		//Get the Final Hash of the Data
		MiniData current = mData;
		
		int len = getProofLen();
		for(int i=0;i<len;i++) {
			ProofChunk chunk = mProofChain.get(i);
			
			if(chunk.getLeft().isTrue()) {
				current = Crypto.getInstance().hashObjects(chunk.getHash(), current, HASH_BITS);
			}else {
				current = Crypto.getInstance().hashObjects(current, chunk.getHash(), HASH_BITS);
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
			jsonchunk.put("value", chunk.getValue().toString());
			
			proof.add(jsonchunk);
		}
		
		json.put("data", mData.to0xString());
		json.put("hashbits", HASH_BITS);
		json.put("proofchain", proof);
		json.put("chainsha", getChainSHAProof().to0xString());
		
		json.put("finalhash", getFinalHash().to0xString());
		
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniByte hb = new MiniByte(HASH_BITS / 32);
		hb.writeDataStream(zOut);
		
		mData.writeDataStream(zOut);
		MiniNumber mlen = new MiniNumber(mProofChain.size());
		mlen.writeDataStream(zOut);
		int len = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			ProofChunk chunk = mProofChain.get(i);
			chunk.getLeft().writeDataStream(zOut);
			chunk.getHash().writeHashToStream(zOut);
			chunk.getValue().writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniByte hb = MiniByte.ReadFromStream(zIn);
		HASH_BITS   = hb.getValue() * 32;
		
		mData = MiniData.ReadFromStream(zIn);
		mProofChain = new ArrayList<>();
		MiniNumber mlen = MiniNumber.ReadFromStream(zIn);
		int len = mlen.getAsInt();
		for(int i=0;i<len;i++) {
			MiniByte left    = MiniByte.ReadFromStream(zIn);
			MiniData hash    = MiniData.ReadHashFromStream(zIn);
			MMRSumNumber val = MMRSumNumber.ReadFromStream(zIn);
			mProofChain.add(new ProofChunk(left, hash, val));
		}
		
		finalizeHash();
	}
	
	public static Proof ReadFromStream(DataInputStream zIn) throws IOException{
		Proof proof = new Proof();
		proof.readDataStream(zIn);
		return proof;
	}
	
	public static int getChainSHABits(String zChainSHA) throws Exception {
		
		//Get the first 4 digits..
		String bits = zChainSHA.substring(0, 4);
		
		//Convert to Decimal.
		int dec = BaseConverter.hexToNumber(bits);
		
		if(dec<5 || dec>16) {
			//ERROR
			throw new Exception("Invalid ChainSHA.. must be 160, 224, 256, 288, 320, 384, 416, 448, 480 or 512");	
		}
		
		//And multiply by 32..
		return dec * 32;
	}
}
