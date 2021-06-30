package org.minima.system.txpow;

import org.minima.GlobalParams;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;

/**
 * The first ever TxPoW will eventually give out the initial coins..
 * 
 * @author spartacusrex
 *
 */
public class GenesisTxPOW extends TxPoW{
	
	public GenesisTxPOW() {
		super();
		
		setTxDifficulty(Crypto.MAX_HASH);
		
		setNonce(new MiniNumber(256));
		 
		setTimeMilli(new MiniNumber(System.currentTimeMillis()));
		
		setBlockNumber(MiniNumber.ZERO);
		
		setBlockDifficulty(Crypto.MAX_HASH);
		
		//Super Block Levels.. FIRST just copy them all..
		MiniData ultimateparent = new MiniData("0x00");
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			setSuperParent(i, ultimateparent);
		}
		
		//Set Transaction and Witness..
		Transaction trans = new Transaction();
		Witness wit       = new Witness();

		//Set the body hash
		setHeaderBodyHash();
		
		//Set the TXPOW
		calculateTXPOWID();
		
		//Hard code it as a block..
		_mIsBlockPOW = true;
		_mIsTxnPOW   = false;
	}
	
}
