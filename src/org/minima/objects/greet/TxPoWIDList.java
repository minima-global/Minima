package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class TxPoWIDList implements Streamable {

	ArrayList<MiniData> mTxPowIDList = new ArrayList<>();
	
	public TxPoWIDList() {}
	
	public void addTxPowID(MiniData zTxPoWID) {
		mTxPowIDList.add(zTxPoWID);
	}

	public ArrayList<MiniData> getList(){
		return mTxPowIDList;
	}
	
	public int size(){
		return mTxPowIDList.size();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len = mTxPowIDList.size();
		MiniNumber minlen = new MiniNumber(len);
		minlen.writeDataStream(zOut);
		
		for(MiniData txpowid : mTxPowIDList) {
			txpowid.writeHashToStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniNumber minlen = MiniNumber.ReadFromStream(zIn);
		int len = minlen.getAsInt();
		
		mTxPowIDList = new ArrayList<>();
		for(int i=0;i<len;i++) {
			mTxPowIDList.add(MiniData.ReadHashFromStream(zIn));
		}
	}
}
