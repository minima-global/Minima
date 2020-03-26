package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniHash;
import org.minima.utils.json.JSONObject;

/**
 * Fee Transaction can have MAX 1 input MAX 1 output and must be Minima
 * @author spartacusrex
 *
 */
public class FeeTransaction extends Transaction {

	/**
	 * This FEE transaction pays for this Transaction only..
	 */
	MiniHash mMainTransaction;
	
	public FeeTransaction(MiniHash zMainTransaction) {
		super();		
		
		mMainTransaction = zMainTransaction;
	}
	
	@Override
	public void addInput(Coin zCoin) {
		if(mInputs.size()<1) {
			super.addInput(zCoin);
		}
	}
	
	@Override
	public void addOutput(Coin zCoin) {
		if(mOutputs.size()<1) {
			super.addInput(zCoin);
		}
	}
	
	@Override
	public JSONObject toJSON() {
		JSONObject json = super.toJSON();
		json.put("transaction", mMainTransaction.to0xString());
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		super.writeDataStream(zOut);
		mMainTransaction.writeDataStream(zOut);
	}
	
	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		super.readDataStream(zIn);
		mMainTransaction = MiniHash.ReadFromStream(zIn);
	}
}
