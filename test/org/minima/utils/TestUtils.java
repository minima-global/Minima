package org.minima.utils;

import org.minima.objects.TxPoW;

public class TestUtils {
	
	public static TxPoW getTxPoW(String zTxPoWID) {
		return getTxPoW(zTxPoWID, 1, 0);
	}
	
	public static TxPoW getTxPoW(String zTxPoWID, int zBlock, int zWeight) {
		TxPoW txp = new TxPoW(zTxPoWID, zBlock, zWeight);
		return txp;
	}
	
}
