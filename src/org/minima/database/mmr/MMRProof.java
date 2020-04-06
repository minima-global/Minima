package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.Proof;
import org.minima.utils.json.JSONObject;

public class MMRProof extends Proof {
	
	/**
	 * The block time this proof points to
	 */
	MiniNumber mBlockTime = new MiniNumber(0);
	
	/**
	 * The Entry number in the MMR
	 */
	MiniInteger mEntryNumber = new MiniInteger(0);
	
	/**
	 * The Provable data
	 */
	MMRData mData;
	
	public MMRProof() {
		super();
	}
		
	public MMRProof(MiniInteger zEntryNumber, MMRData zInitialData, MiniNumber zBlockTime) {
		mEntryNumber = zEntryNumber;
		mData        = zInitialData;
		mBlockTime   = zBlockTime;
		
		setData(mData.getFinalHash());
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public MiniInteger getEntryNumber() {
		return mEntryNumber;
	}
	
	public MMRData getMMRData() {
		return mData;
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
		boolean coinidcheck  = cc.getCoinID().isEqual(zCoin.getCoinID());
		boolean amountcheck  = cc.getAmount().isEqual(zCoin.getAmount());
		boolean addresscheck = cc.getAddress().isEqual(zCoin.getAddress());
		boolean tokencheck   = cc.getTokenID().isEqual(zCoin.getTokenID());
		
		return coinidcheck && amountcheck && addresscheck && tokencheck;
	}
	
	@Override
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject(); 
		
		obj.put("blocktime", mBlockTime.toString());
		obj.put("entry", mEntryNumber.toString());
		obj.put("data", mData.toJSON());
		obj.put("proof",super.toJSON());
		
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
		
		super.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		mEntryNumber = MiniInteger.ReadFromStream(zIn);
		mData        = MMRData.ReadFromStream(zIn);
		
		super.readDataStream(zIn);
	}
	
	public static MMRProof ReadFromStream(DataInputStream zIn){
		MMRProof proof = new MMRProof();
		
		try {
			proof.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return proof;
	}
}
