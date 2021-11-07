package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class CoinProof implements Streamable {

	/**
	 * The Coin record in the MMR
	 */
	Coin mCoin;
	
	/**
	 * The proof of this record
	 */
	MMRProof mProof;
	
	private CoinProof() {}
	
	public CoinProof(Coin zCoin, MMRProof zProof) {
		mCoin 		= zCoin;
		mProof 		= zProof;
	}
	
	public Coin	getCoin() {
		return mCoin;
	}

	public MMRProof getMMRProof(){
		return mProof;
	}
	
	public MMRData getMMRData() {
		
		//Get the Hash of this 
		MiniData hash 		= Crypto.getInstance().hashObject(getCoin());
		
		//The Value
		MiniNumber value 	= getCoin().getAmount();
		
		return new MMRData(hash, value);
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		ret.put("coin", mCoin.toJSON());
		ret.put("proof", mProof.toJSON());
		return ret;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCoin.writeDataStream(zOut);
		mProof.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoin	= Coin.ReadFromStream(zIn);
		mProof 	= MMRProof.ReadFromStream(zIn);
	}
	
	public static CoinProof ReadFromStream(DataInputStream zIn) throws IOException {
		CoinProof cp = new CoinProof();
		cp.readDataStream(zIn);
		return cp;
				
	}
}
