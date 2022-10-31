package org.minima.database.txpowdb.sql;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class TxPoWList implements Streamable {

	public ArrayList<TxPoW> mTxPoWs = new ArrayList<>();
	
	private TxPoWList() {}
	
	public TxPoWList(ArrayList<TxPoW> zTxPoWs) {
		mTxPoWs = zTxPoWs;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeInt(mTxPoWs.size());
		for(TxPoW txp : mTxPoWs) {
			txp.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTxPoWs = new ArrayList<>();
		int size = zIn.readInt();
		for(int i=0;i<size;i++) {
			TxPoW txp = TxPoW.ReadFromStream(zIn);
			mTxPoWs.add(txp);
		}
	}
	
	/**
	 * Convert a MiniData version into a Coin
	 */
	public static TxPoWList convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		TxPoWList txnrow = null;
		
		try {
			//Convert data
			txnrow = TxPoWList.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return txnrow;
	}
	
	public static TxPoWList ReadFromStream(DataInputStream zIn) throws IOException {
		TxPoWList txplist = new TxPoWList();
		txplist.readDataStream(zIn);
		return txplist;
	}
}
