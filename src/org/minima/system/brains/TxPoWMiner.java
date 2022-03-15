package org.minima.system.brains;

import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxHeader;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.utils.Crypto;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class TxPoWMiner extends MessageProcessor {

	public static final String TXPOWMINER_MINETXPOW 	= "TXPOWMINER_MINETXPOW";
	public static final String TXPOWMINER_MINEPULSE 	= "TXPOWMINER_MINEPULSE";
	
	/**
	 * The Large Byte MiniNumber to set the Header up for hashing
	 */
	private static MiniNumber START_NONCE_BYTES = new MiniNumber("100000000000000000.00000000000000000000000000000000000000001");
	
	/**
	 * A list of coins currently being mined.. to check when creating new transactions 
	 */
	ArrayList<String> mMiningCoins;
	
	public TxPoWMiner() {
		super("MINER");
		
		mMiningCoins = new ArrayList<>();
	}
	
	public void mineTxPoW(TxPoW zTxPoW) {
		
		//Add these coins to our Mining list
		addMiningCoins(zTxPoW);
		
		//Now post a Mining message
		PostMessage(new Message(TXPOWMINER_MINETXPOW).addObject("txpow", zTxPoW));
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(TXPOWMINER_MINETXPOW)) {
			
			//Get the TxPoW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Hard set the Header Body hash - now we are mining it can never change
			txpow.setHeaderBodyHash();
			
			//Set the nonce.. we make it a large size in bytes then edit those - no reserialisation
			txpow.setNonce(START_NONCE_BYTES);
			
			//Post a message.. Mining Started
			Message mining = new Message(Main.MAIN_MINING);
			mining.addBoolean("starting", true);
			mining.addObject("txpow", txpow);
			Main.getInstance().PostMessage(mining);
			
			//Get the byte data
			byte[] data = MiniData.getMiniDataVersion(txpow.getTxHeader()).getBytes();
			
			//Cycle until done..
			MiniNumber finalnonce 	= MiniNumber.ZERO;
			BigInteger newnonce 	= BigInteger.ZERO;
			while(isRunning()) {
				
				//Get a nonce to write over the data
				byte[] noncebytes = newnonce.toByteArray();
				newnonce 		  = newnonce.add(BigInteger.ONE);
				
				//Copy these into the byte array of the TxHeader 
				//start 2 numbers in so leading zero is not changed
				System.arraycopy(noncebytes, 0, data, 4, noncebytes.length);
				
				//Hash the data array
				byte[] hashedbytes = Crypto.getInstance().hashData(data);
				
				//Make into a MiniData structure
				MiniData hash = new MiniData(hashedbytes);
				
				//Have we found a valid txpow
				if(hash.isLess(txpow.getTxnDifficulty())) {
					
					//Ok read in the final data..
					MiniData finaldata = new MiniData(data);
					
					//Now convert to a TxHeader
					TxHeader txh = TxHeader.convertMiniDataVersion(finaldata);
					
					//What was the nonce..
					finalnonce = txh.mNonce;
					
					break;
				}
			}
			
			//Now set the final nonce..
			txpow.setNonce(finalnonce);
			
			//Calculate TxPoWID
			txpow.calculateTXPOWID();
			
			//Post a message.. Mining Finished
			Message miningend = new Message(Main.MAIN_MINING);
			miningend.addBoolean("starting", false);
			miningend.addObject("txpow", txpow);
			Main.getInstance().PostMessage(miningend);
			
			//Post it on..
			Main.getInstance().PostMessage(new Message(Main.MAIN_TXPOWMINED).addObject("txpow", txpow));
		
			//Remove the coins from our mining list
			removeMiningCoins(txpow);
			
		}else if(zMessage.isMessageType(TXPOWMINER_MINEPULSE)) {
			
			//Do we have any blocks yet
			if(MinimaDB.getDB().getTxPoWTree().getTip() != null) {
				
				//Get the current TxPoW
				TxPoW txpow = TxPoWGenerator.generateTxPoW(new Transaction(), new Witness());
				
				//Mine a TxPow..
				PostMessage(new Message(TXPOWMINER_MINETXPOW).addObject("txpow", txpow).addBoolean("automine", true));
			}
		}
	}

	/**
	 * Add these coins to a list so we know we can;t use them when creating a transaction..
	 */
	private void addMiningCoins(TxPoW zTxPoW) {
		
		//Get all the coins..
		if(!zTxPoW.getTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getTransaction().getAllInputs();
			for(Coin cc : inputs) {
				String coinid = cc.getCoinID().to0xString();
				if(!mMiningCoins.contains(coinid)) {
					mMiningCoins.add(coinid);
				}
			}
		}
		
		if(!zTxPoW.getBurnTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getBurnTransaction().getAllInputs();
			for(Coin cc : inputs) {
				String coinid = cc.getCoinID().to0xString();
				if(!mMiningCoins.contains(coinid)) {
					mMiningCoins.add(coinid);
				}
			}
		}
	}
	
	private void removeMiningCoins(TxPoW zTxPoW) {
		
		//Get all the coins..
		if(!zTxPoW.getTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getTransaction().getAllInputs();
			for(Coin cc : inputs) {
				mMiningCoins.remove(cc.getCoinID().to0xString());
			}
		}
		
		if(!zTxPoW.getBurnTransaction().isEmpty()) {
			ArrayList<Coin> inputs = zTxPoW.getBurnTransaction().getAllInputs();
			for(Coin cc : inputs) {
				mMiningCoins.remove(cc.getCoinID().to0xString());
			}
		}
	}
	
	public boolean checkForMiningCoin(String zCoinID) {
		return mMiningCoins.contains(zCoinID);
	}
	
	/**
	 * Calculate the Hash rate of this node...
	 */
	public static MiniNumber calculateHashRate() {
		
		MiniNumber hashes 	= MiniNumber.MILLION;
		int ihashes 		= hashes.getAsInt();
		
		long timestart = System.currentTimeMillis();
		MiniData data = MiniData.getRandomData(32);
		for(int i=0;i<ihashes;i++) {
			data = Crypto.getInstance().hashObject(data);
		}
		long timediff = System.currentTimeMillis() - timestart;
		
		MiniNumber timesecs = new MiniNumber(timediff).div(MiniNumber.THOUSAND);
		
		MiniNumber spd = hashes.div(timesecs);
		
		return spd;
	}
}
