package org.minima.system.bootstrap;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;

import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.tx.TXMiner;
import org.minima.utils.Crypto;
import org.minima.utils.SuperBlockLevels;

public class GenesisTxPOW extends TxPOW{
	
	public GenesisTxPOW() {
		super();
		
		setTxDifficulty(Crypto.MAX_HASH);
		
		setNonce(new MiniInteger(256));
		 
		setTimeSecs(new MiniNumber(""+(System.currentTimeMillis()/1000)));
		
		setBlockNumber(MiniNumber.ZERO);
		
		setBlockDifficulty(Crypto.MAX_HASH);
		
		setParent(new MiniData("0x00"));
		
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
		
		//Set the TXPOW
		calculateTXPOWID();
		
		//Hard code it as a block..
		_mIsBlockPOW = true;
		_mIsTxnPOW   = false;
	}
	
	
	public static void main(String[] zArgs) {
		GenesisTxPOW gen = new GenesisTxPOW();
		
		try {
			System.out.println("GEN 1 : "+gen);
			
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			
			gen.writeDataStream(dos);
			
			dos.flush();
			
			byte[] data = baos.toByteArray();
			
			ByteArrayInputStream bais = new ByteArrayInputStream(data);
			DataInputStream dis = new DataInputStream(bais);
			
			TxPOW tp = new TxPOW();
			tp.readDataStream(dis);
			
			System.out.println("GEN 2 : "+tp);
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
	}

}
