package org.minima.system.brains;

import org.minima.database.MinimaDB;
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
	
	public TxPoWMiner() {
		super("MINER");
	}
	
	public void mineTxPoW(TxPoW zTxPoW) {
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
			
			//Post a message..
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
			
			//Post it on..
			Main.getInstance().PostMessage(new Message(Main.MAIN_TXPOWMINED).addObject("txpow", txpow));
		
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

}
