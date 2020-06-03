package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class TxPoWList implements Streamable {

	ArrayList<TxPoW> mTxPowList = new ArrayList<>();
	
	public TxPoWList() {}
	
	public void addTxPow(TxPoW zTxPoW) {
		mTxPowList.add(0,zTxPoW);
	}

	public ArrayList<TxPoW> getList(){
		return mTxPowList;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len = mTxPowList.size();
		MiniNumber minlen = new MiniNumber(len);
		minlen.writeDataStream(zOut);
		
		for(TxPoW txpow : mTxPowList) {
			txpow.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniNumber minlen = MiniNumber.ReadFromStream(zIn);
		int len = minlen.getAsInt();
		
		mTxPowList = new ArrayList<>();
		for(int i=0;i<len;i++) {
			TxPoW txp = new TxPoW();
			txp.readDataStream(zIn);
			mTxPowList.add(txp);
		}
	}
}
