package org.minima.database.mmr;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MMRProof implements Streamable {
	
	/**
	 * Each node in the proof
	 */
	public class MMRProofChunk  implements Streamable {
		MiniByte 	mLeft;
		MMRData		mMMRData;
		
		public MMRProofChunk() {}
		
		public MMRProofChunk(boolean zIsLeft, MMRData zData) {
			mLeft 	= new MiniByte(zIsLeft);
			mMMRData 	= zData;
		}
		
		public boolean isLeft() {
			return mLeft.isTrue();
		}
		
		public MMRData getMMRData() {
			return mMMRData;
		}
		
		public JSONObject toJSON() {
			JSONObject json = new JSONObject();
			json.put("left", mLeft.isTrue());
			json.put("data", mMMRData.toJSON());
			return json;
		}
		
		@Override
		public void writeDataStream(DataOutputStream zOut) throws IOException {
			mLeft.writeDataStream(zOut);
			mMMRData.writeDataStream(zOut);
		}

		@Override
		public void readDataStream(DataInputStream zIn) throws IOException {
			mLeft 		= MiniByte.ReadFromStream(zIn);
			mMMRData 	= MMRData.ReadFromStream(zIn);
		}
	}

	/**
	 * The block time this proof points to
	 */
	protected MiniNumber mBlockTime = new MiniNumber(0);
	
	/**
	 * The Merkle Branch
	 */
	protected ArrayList<MMRProofChunk> mProofChain = new ArrayList<>();
	
	public MMRProof() {
		mBlockTime   = MiniNumber.ZERO;
	}
		
	public MMRProof(MiniNumber zBlockTime) {
		mBlockTime   = zBlockTime;
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public void addProofChunk(MMRProofChunk zChunk) {
		mProofChain.add(zChunk);
	}
	
	public void addProofChunk(boolean zIsLeft, MMRData zData) {
		mProofChain.add(new MMRProofChunk(zIsLeft, zData));
	}
	
	public MMRProofChunk getProofChunk(int zProofIndex) {
		return mProofChain.get(zProofIndex);
	}
	
	public int getProofLength() {
		return mProofChain.size();
	}
	
	public MMRData calculateProof(MMRData zData) {
		//Get the Final Hash of the Data
		MiniData cdata  	= zData.getData();
		MiniNumber cvalue 	= zData.getValue();
		
		//Cycle through the whole proof..
		for(MMRProofChunk proofchunk : mProofChain) {
			
			//Add to the value..
			cvalue = cvalue.add(proofchunk.getMMRData().getValue());
			
			//Hash the children..
			if(proofchunk.isLeft()) {
				cdata = Crypto.getInstance().hashAllObjects(proofchunk.getMMRData().getData(), cdata, cvalue);
			}else {
				cdata = Crypto.getInstance().hashAllObjects(cdata, proofchunk.getMMRData().getData(), cvalue);
			}
		}
		
		return new MMRData(cdata, cvalue);
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject(); 
		
		obj.put("blocktime", mBlockTime.toString());
		
		JSONArray arr = new JSONArray();
		for(MMRProofChunk chunk : mProofChain) {
			arr.add(chunk.toJSON());
		}
		
		obj.put("proof",arr);
		obj.put("prooflength",mProofChain.size());
		
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		//BlockTime first
		mBlockTime.writeDataStream(zOut);
		
		//No need to add value in the MMRData - just set to ZERO (script and signature proofs)
		int len = mProofChain.size();
		MiniNumber.WriteToStream(zOut, len);
		for(int i=0;i<len;i++) {
			MMRProofChunk chunk = getProofChunk(i);
			chunk.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		//BlockTime first
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		
		mProofChain = new ArrayList<>();
		MiniNumber plen = MiniNumber.ReadFromStream(zIn);
		int len = plen.getAsInt();
		for(int i=0;i<len;i++) {
			MMRProofChunk chunk = new MMRProofChunk();
			chunk.readDataStream(zIn);
			addProofChunk(chunk);
		}
	}
	
	public static MMRProof ReadFromStream(DataInputStream zIn) throws IOException{
		MMRProof proof = new MMRProof();
		proof.readDataStream(zIn);
		return proof;
	}
	
	/**
	 * Convert a MiniData version into an MMRProof
	 */
	public static MMRProof convertMiniDataVersion(MiniData zMMRProof) throws IOException {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zMMRProof.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		//read in the proof
		MMRProof proof = MMRProof.ReadFromStream(dis);
	
		dis.close();
		bais.close();
		
		return proof;
	}
}
