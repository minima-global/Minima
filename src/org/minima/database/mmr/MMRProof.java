package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MMRProof implements Streamable {
	
	/**
	 * The block time this proof points to
	 */
	MiniNumber mBlockTime = MiniNumber.ZERO;
	
	/**
	 * The Entry number in the MMR
	 */
	MiniNumber mEntryNumber = MiniNumber.ZERO;
	
	/**
	 * The Provable data
	 */
	MMRData mData;
	
	/**
	 * The list of Hash values.. Left or right can be worked out from the original Entry Number..
	 */
	ArrayList<MiniHash> mProofChain;
	ArrayList<MiniByte>   mLeftHash;
	
	public MMRProof() {
		mProofChain = new ArrayList<>();
		mLeftHash   = new ArrayList<>();
	}
		
	public MMRProof(MiniNumber zEntryNumber, MMRData zInitialData, MiniNumber zBlockTime) {
		mProofChain = new ArrayList<>();
		mLeftHash   = new ArrayList<>();
		mEntryNumber = zEntryNumber;
		mData        = zInitialData;
		mBlockTime   = zBlockTime;
	}
	
	public void addHash(MiniHash zHash, boolean zLeft) {
		mProofChain.add(zHash);	
		if(zLeft) {
			mLeftHash.add(MiniByte.TRUE);
		}else {
			mLeftHash.add(MiniByte.FALSE);
		}
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public MiniNumber getEntryNumber() {
		return mEntryNumber;
	}
	
	public MMRData getMMRData() {
		return mData;
	}
	
	public MiniByte getLeftHash(int zProof) {
		return mLeftHash.get(zProof);
	}
	
	public MiniHash getProof(int zProof) {
		return mProofChain.get(zProof);
	}
	
	public int getProofLen() {
		return mProofChain.size();
	}
	
	public MiniHash calculateProof() {
		//Get the Final Hash of the Data
		MiniHash current = mData.getFinalHash();
		
		int len = getProofLen();
		for(int i=0;i<len;i++) {
			if(getLeftHash(i).isEqual(MiniByte.TRUE)) {
				current = Crypto.getInstance().hashObjects(getProof(i), current);
			}else {
				current = Crypto.getInstance().hashObjects(current, getProof(i));
			}
		}
		return current;
	}
	
	/**
	 * Check this proof is the same as this coin..
	 * 
	 * @param zCoin
	 * @return
	 */
	public boolean checkCoin(Coin zCoin) {
		//Check Against
		Coin cc = getMMRData().getCoin();
		
		//Is this input for the correct details..
		boolean coinidcheck  = cc.getCoinID().isExactlyEqual(zCoin.getCoinID());
		boolean amountcheck  = cc.getAmount().isEqual(zCoin.getAmount());
		boolean addresscheck = cc.getAddress().isExactlyEqual(zCoin.getAddress());
		boolean tokencheck   = cc.getTokenID().isExactlyEqual(zCoin.getTokenID());
		
		return coinidcheck && amountcheck && addresscheck && tokencheck;
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject(); 
		
		obj.put("blocktime", mBlockTime.toString());
		obj.put("entry", mEntryNumber.toString());
		obj.put("data", mData.toJSON());
		
		JSONArray proof = new JSONArray();
		int len = mProofChain.size();
		for(int i=0;i<len;i++){
			JSONObject chunk = new JSONObject();
			chunk.put("index", i);
			chunk.put("leftside", mLeftHash.get(i).isTrue());
			chunk.put("hash", mProofChain.get(i).toString());
			
			proof.add(chunk);
		}
		obj.put("proofchain", proof);
		
		obj.put("finalhash", calculateProof().toString());
		
		return obj;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mBlockTime.writeDataStream(zOut);
		mEntryNumber.writeDataStream(zOut);
		
		mData.writeDataStream(zOut);
		
		int len = mProofChain.size();
		zOut.writeInt(len);
		for(int i=0;i<len;i++) {
			getLeftHash(i).writeDataStream(zOut);
			getProof(i).writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		mEntryNumber = MiniNumber.ReadFromStream(zIn);
		mData        = MMRData.ReadFromStream(zIn);
		
		mProofChain  = new ArrayList<>();
		mLeftHash    = new ArrayList<>();
		
		int len = zIn.readInt();
		for(int i=0;i<len;i++) {
			mLeftHash.add(MiniByte.ReadFromStream(zIn));
			mProofChain.add(MiniHash.ReadFromStream(zIn));
		}
	}
	
	public static MMRProof ReadFromStream(DataInputStream zIn){
		MMRProof proof = new MMRProof();
		
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
