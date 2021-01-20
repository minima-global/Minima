package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Magic implements Streamable {

	/**
	 * A Random Magic number so that everyone is working on a different TxPoW in the pulse 
	 * (since there is no coinbase..)
	 */
	public MiniData mPRNG = MiniData.getRandomData(64);
	
	/**
	 * The MAGIC numbers.. set by chain vote avg over the last 4096 blocks..
	 */
	public MiniNumber mDesiredMaxTxPoWSize          = new MiniNumber(20000);
	public MiniNumber mDesiredMaxTxnPerBlock        = new MiniNumber(32);
	public MiniNumber mDesiredMaxKISSVMInstructions = new MiniNumber(128);
	
	public Magic() {}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("prng", mPRNG.to0xString());
		magic.put("maxtxpow", mDesiredMaxTxPoWSize.getAsInt());
		magic.put("maxtxn", mDesiredMaxTxnPerBlock.getAsInt());
		magic.put("maxkissvm", mDesiredMaxKISSVMInstructions.getAsInt());
		
		return magic;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mPRNG.writeHashToStream(zOut);
		mDesiredMaxTxPoWSize.writeDataStream(zOut);
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		mDesiredMaxKISSVMInstructions.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPRNG = MiniData.ReadHashFromStream(zIn);
		mDesiredMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxKISSVMInstructions = MiniNumber.ReadFromStream(zIn);
	}
	
}
