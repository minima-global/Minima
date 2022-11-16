package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxBody implements Streamable {

	/**
	 * A Random number so that everyone is working on a different TxPoW in the pulse 
	 * (since there is no coinbase..)
	 */
	public MiniData 	mPRNG = MiniData.getRandomData(32);

	/**
	 * The Difficulty for this TXPOW to be valid.
	 */
	public MiniData 	mTxnDifficulty = Crypto.MAX_HASH;
	
	/**
	 * The Transaction the user is trying to send
	 */
	public Transaction	mTransaction = new Transaction();
	
	/**
	 * The Witness data for the Transaction
	 */
	public Witness		mWitness = new Witness();
	
	/**
	 * The BURN paying for the Transaction the user is trying to send - can be empty
	 */
	public Transaction	mBurnTransaction = new Transaction();
	
	/**
	 * The Witness data for the FeeTransaction - can be empty
	 */
	public Witness	   mBurnWitness = new Witness();
	
	/**
	 * The list of the current TX-POWs the user 
	 * knows about that are not yet in the this chain.
	 */
	public ArrayList<MiniData> mTxPowIDList;
	
	public TxBody() {
		//List of the transctions in this block
		mTxPowIDList = new ArrayList<>();
	}

	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("prng", mPRNG.to0xString());
		
		txpow.put("txndiff", mTxnDifficulty.to0xString());
		
		txpow.put("txn", mTransaction.toJSON());
		txpow.put("witness", mWitness.toJSON());
		
		//The BURN transaction.. normally empty
		txpow.put("burntxn", mBurnTransaction.toJSON());
		txpow.put("burnwitness", mBurnWitness.toJSON());
		
		//Need to make it into a JSON array
		JSONArray txns = new JSONArray();
		for(MiniData txn : mTxPowIDList) {
			txns.add(txn.to0xString());
		}
		txpow.put("txnlist", txns);
		
		return txpow;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPRNG.writeHashToStream(zOut);
		
		mTxnDifficulty.writeDataStream(zOut);
		mTransaction.writeDataStream(zOut);
		mWitness.writeDataStream(zOut);
		mBurnTransaction.writeDataStream(zOut);
		mBurnWitness.writeDataStream(zOut);
		
		//Write out the TXPOW List
		int len = mTxPowIDList.size();
		MiniNumber ramlen = new MiniNumber(""+len);
		ramlen.writeDataStream(zOut);
		for(MiniData txpowid : mTxPowIDList) {
			txpowid.writeHashToStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPRNG = MiniData.ReadHashFromStream(zIn);
		
		mTxnDifficulty  = MiniData.ReadFromStream(zIn);
		mTransaction.readDataStream(zIn);
		mWitness.readDataStream(zIn);
		mBurnTransaction.readDataStream(zIn);
		mBurnWitness.readDataStream(zIn);
		
		//Read in  the TxPOW list
		mTxPowIDList = new ArrayList<>();
		MiniNumber ramlen = MiniNumber.ReadFromStream(zIn);
		int len = ramlen.getAsInt();
		for(int i=0;i<len;i++) {
			mTxPowIDList.add(MiniData.ReadHashFromStream(zIn));
		}
	}
}
