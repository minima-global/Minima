package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
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
			
			//The Start Nonce..
			MiniNumber nonce = new MiniNumber(0);
			
			//Set the Time..
			txpow.setTimeMilli(new MiniNumber(System.currentTimeMillis()));
			
			//And now start hashing.. 
			MiniData hash   = null;
			
			//Post a message.. Mining Started
			Message mining = new Message(Main.MAIN_MINING);
			mining.addBoolean("starting", true);
			mining.addObject("txpow", txpow);
			Main.getInstance().PostMessage(mining);
			
			//Cycle until done..
			while(isRunning()) {
				
				//Set the nonce..
				txpow.setNonce(nonce);
				
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow.getTxHeader());
				
				//Have we found a valid txpow
				if(hash.isLess(txpow.getTxnDifficulty())) {
					break;
				}
				
				//Increment the nonce..
				nonce = nonce.add(MiniNumber.MINI_UNIT);
			}
			
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
	
}
