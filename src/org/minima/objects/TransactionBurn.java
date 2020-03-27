package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
import org.minima.utils.json.JSONObject;

/**
 * Fee Transaction can have MAX 1 input MAX 1 output and must be Minima
 * @author spartacusrex
 *
 */
public class TransactionBurn extends Transaction {

	/**
	 * This FEE transaction pays for this Transaction only..
	 */
	MiniHash mMainTransaction = MiniHash.ZERO32;
	
	public TransactionBurn() {
		super();		
	}
	
	public void setTransaction(MiniHash zTransaction){
		mMainTransaction = zTransaction;
	}
	
	public MiniHash getMainTransactionHash() {
		return mMainTransaction;
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
	public boolean checkValidInOutPerToken(){
		if(mInputs.size()!=1) {
			return false;
		}
		
		if(!mInputs.get(0).getTokenID().isExactlyEqual(Coin.MINIMA_TOKENID)) {
			return false;
		}
		
		if(mOutputs.size()>1) {
			return false;
		}
		
		return super.checkValidInOutPerToken();
	}
	
	@Override
	public JSONObject toJSON() {
		JSONObject json = super.toJSON();
		
		if(mMainTransaction.isExactlyEqual(MiniHash.ZERO32)) {
			json.put("transaction", "0x00");	
		}else {
			json.put("transaction", mMainTransaction.to0xString());	
		}
		
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		super.writeDataStream(zOut);
		
		//Mo pOint wasting space..
		if(mMainTransaction.isExactlyEqual(MiniHash.ZERO32)) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mMainTransaction.writeDataStream(zOut);
		}
		
	}
	
	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		super.readDataStream(zIn);
		
		boolean valid = MiniByte.ReadFromStream(zIn).isTrue();
		if(valid) {
			mMainTransaction = MiniHash.ReadFromStream(zIn);
		}else {
			mMainTransaction = MiniHash.ZERO32;
		}
	}
}
