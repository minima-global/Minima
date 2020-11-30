package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class TxPoWList implements Streamable {

	//Is this list a crossover just appended to your current or a totally new IBD
	boolean mCrossover = false;
	
	ArrayList<TxPoW> mTxPowList = new ArrayList<>();
	
	public TxPoWList() {}
	
	public void addTxPow(TxPoW zTxPoW) {
		mTxPowList.add(zTxPoW);
	}

	public ArrayList<TxPoW> getList(){
		return mTxPowList;
	}
	
	public int size() {
		return mTxPowList.size();
	}
	
	
	public boolean isCrossover() {
		return mCrossover;
	}
	
	public void setCrossOver(boolean zCrossOver) {
		mCrossover = zCrossOver;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len = mTxPowList.size();
		MiniNumber minlen = new MiniNumber(len);
		minlen.writeDataStream(zOut);
		
		for(TxPoW txpow : mTxPowList) {
			txpow.writeDataStream(zOut);
		}
		
		if(isCrossover()) {
			MiniByte.TRUE.writeDataStream(zOut);	
		}else {
			MiniByte.FALSE.writeDataStream(zOut);
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
		
		mCrossover = MiniByte.ReadFromStream(zIn).isTrue();
	}
}
