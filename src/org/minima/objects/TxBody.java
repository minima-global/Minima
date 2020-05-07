package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import org.minima.GlobalParams;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class TxBody implements Streamable {

	/**
	 * The Difficulty for this TXPOW to be valid.
	 */
	private MiniData 	mTxnDifficulty = new MiniData();
	
	/**
	 * The Transaction the user is trying to send
	 */
	private Transaction	mTransaction = new Transaction();
	
	/**
	 * The Witness data for the Transaction
	 */
	private Witness		mWitness = new Witness();
	
	/**
	 * The BURN paying for the Transaction the user is trying to send - can be empty
	 */
	private Transaction	mBurnTransaction = new Transaction();
	
	/**
	 * The Witness data for the FeeTransaction - can be empty
	 */
	private Witness	   mBurnWitness = new Witness();
	
	/**
	 * The list of the current TX-POWs the user 
	 * knows about that are not yet in the this chain.
	 */
	private ArrayList<MiniData> mTxPowIDList;
	
	/**
	 * The MMR Root!
	 */
	public MiniData mMMRRoot = new MiniData();
	
	/**
	 * The Total Sum Of All coins in the system
	 */
	public MMRSumNumber mMMRTotal = MMRSumNumber.ZERO;
	
	/**
	 * A Random Magic number so that everyone is working on a different TxPOW in the pulse 
	 * (since there is no coinbase..)
	 */
	public MiniData mMagic = MiniData.getRandomData(32);
	
	/**
	 * A Custom Hash. Can be anything the user wants..
	 */
	public MiniData mCustom = new MiniData("0x00");
	
	
	public TxBody() {
		//List of the transctions in this block
		mTxPowIDList = new ArrayList<>();
		
	}

	public JSONObject toJSON() {
		JSONObject txpow = new JSONObject();
		
		txpow.put("txndiff", mTxnDifficulty.to0xString());
		txpow.put("txn", mTransaction.toJSON());
		txpow.put("txnid", getTransID().to0xString());
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
		
		txpow.put("magic", mMagic.toString());
		txpow.put("custom", mCustom.toString());
		
		txpow.put("mmr", mMMRRoot.toString());
		txpow.put("total", mMMRTotal.toString());
		
		return txpow;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
	}


	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
	}
}
