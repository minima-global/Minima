package org.minima.system.txpow;

import org.minima.GlobalParams;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
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
		
		setNonce(new MiniInteger(256));
		 
		setTimeMilli(new MiniNumber(""+System.currentTimeMillis()));
		
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

//		Coin in = new Coin(GENESIS_INPUT,new Address("RETURN TRUE"),new MiniNumber(50));
//		trans.addInput(in);
//		wit.addParam("");
//		
//		//And send to the new address
//		Address outaddr = new Address(new MiniData32(MiniData.getRandomData(32).getData()));
//		Coin out = new Coin(Coin.COINID_OUTPUT,outaddr,new MiniNumber(50));
//		trans .addOutput(out);
		
		//Set transaction
		setTransaction(trans);
		setWitness(wit);
		
//		//Calculate the Ouput COINID.. for the MMR..
//		MiniData32 transhash = Crypto.getInstance().hashObject(trans);
//				
//		//Now calculate the CoinID / TokenID
//		MiniData32 coinid    = Crypto.getInstance().hashObjects(transhash, new MiniByte(0));
//		
//		//Calcualte the MMR..
//		MMR mmr = new MMR();
//		
//		//Add that CoinID.. There are no other txns in the genesis TXPOW
//		mmr.insertData(coinid, MiniNumber.ZERO);
//		
//		//Now get the Peaks..
//		MMRState mmrstate = mmr.getMMRState();
//		
//		//Now add to the TXPOW..
//		setMMRState(mmrstate);
		
//		mSuperParents[0] = new MiniData(MiniData.getRandomData(32).getData());
		
		//Set the body hash
		setHeaderBodyHash();
		
		//Set the TXPOW
		calculateTXPOWID();
		
		//Hard code it as a block..
		_mIsBlockPOW = true;
		_mIsTxnPOW   = false;
	}
	
}
