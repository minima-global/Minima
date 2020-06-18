package org.minima.system.txpow;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.system.input.InputHandler;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class TxPoWMiner extends SystemHandler{
	
	public static final MiniData BASE_TXN 	= Crypto.MEGA_HASH;
//	public static final MiniData BASE_TXN 	= Crypto.MAX_HASH;
	public static final MiniData BASE_BLOCK = Crypto.MAX_HASH;
	
	public static final String TXMINER_TESTHASHING = "MINE_TESTHASHING";
	public static final String TXMINER_MINETXPOW   = "MINE_MINETXPOW";
	public static final String TXMINER_MEGAMINER   = "MINE_MEGAMINER";
	
	//Mine a single Block
	public static final String TXMINER_DEBUGBLOCK   = "MINE_DEBUGBLOCK";
	
	boolean mAutoMining = false;
	
	boolean mShowTXPOWMine = true;
	
	public TxPoWMiner(Main zMain) {
		super(zMain,"TXMINER");
	}
	
	public void setAutoMining(boolean zMining) {
		mAutoMining = zMining;
	}
	
	public boolean isAutoMining() {
		return mAutoMining;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(TXMINER_MINETXPOW)) {
			//Get TXPOW..
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Hard set the Header Body hash - now we are mining it can never change
			txpow.setHeaderBodyHash();
			
			//The Start Nonce..
			MiniInteger nonce = new MiniInteger(0);
			
			//And now start hashing.. 
			MiniData hash = null;
			boolean mining 	= true;
			
			//Do so many then recalculate.. to have the latest block data
			long currentTime  = System.currentTimeMillis();
			
			//should be about 10..
			long maxTime  	  = currentTime + 5000;
			
			if(mShowTXPOWMine) {
				MinimaLogger.log("START TXPOW MINING "+txpow.getTransaction());
			}
			
			while(mining && currentTime < maxTime && isRunning()) {
				//Set the Nonce..
				txpow.setNonce(nonce);

				//Set the Time..
				txpow.setTimeMilli(new MiniNumber(""+currentTime));
				
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow.getTxHeader());
				
				if(hash.isLess(txpow.getTxnDifficulty())) {
					//For Now..
					mining = false;
					break;
				}
				
				//Increment the nonce..
				nonce = nonce.increment();
				
				//New time
				currentTime  = System.currentTimeMillis();
			}
			
			//Did we find it.. ?
			if(mining) {
				if(mShowTXPOWMine) {
					MinimaLogger.log("NOTFINISHED "+nonce);
				}
				
				//Repost the same transaction.. get a new TxPOW block with latest details
				Message sametr = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
										.addObject("transaction", txpow.getTransaction())
										.addObject("witness", txpow.getWitness());

				//Send it..
				getMainHandler().getConsensusHandler().PostMessage(sametr);
				
			}else {
				if(mShowTXPOWMine) {
					MinimaLogger.log("TXPOW MINED!");
				}
				
				//Set the TxPOW
				txpow.calculateTXPOWID();
				
				//We have a valid TX-POW..
				Message msg = new Message(ConsensusHandler.CONSENSUS_FINISHED_MINE).addObject("txpow", txpow);
				getMainHandler().getConsensusHandler().PostMessage(msg);
			}
			
		}else if(zMessage.isMessageType(TXMINER_MEGAMINER)) {
			//Get TXPOW..
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Hard set the Header Body hash - now we are mining it can never change
			txpow.setHeaderBodyHash();
			
			//Do so many then recalculate.. to have the latest block data
			long currentTime  = System.currentTimeMillis();
			
			//should be about 10..
			long maxTime  	  = currentTime + 2000;
			
			//Keep cycling until it is ready 
			boolean mining = true;
			MiniData hash = null;
			while(mining && currentTime<maxTime && isRunning()) {
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow.getTxHeader());
				
				//Success ?
				if(hash.isLess(txpow.getBlockDifficulty())) {
					mining = false;
				}else {
					//Set the Nonce..
					txpow.setNonce(txpow.getNonce().increment());
					
					//Set the Time..
					txpow.setTimeMilli(new MiniNumber(""+currentTime));
				}
				
				//New time
				currentTime  = System.currentTimeMillis();
			}
			
			if(!isRunning()) {
				return;
			}

			//Set all the correct internal variables..
			txpow.calculateTXPOWID();
			
			if(txpow.isBlock()) {
				Message msg = new Message(ConsensusNet.CONSENSUS_NET_CHECKSIZE_TXPOW).addObject("txpow", txpow);
				getMainHandler().getConsensusHandler().PostMessage(msg);
			}
			
			//Pause for breath
			Thread.sleep(200);
			
			//And start the whole Mining thing again..
			getMainHandler().getConsensusHandler().PostMessage(ConsensusHandler.CONSENSUS_MINEBLOCK);
			
		}else if(zMessage.isMessageType(TXMINER_DEBUGBLOCK)) {
			//Get TXPOW..
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Hard set the Header Body hash - now we are mining it can never change
			txpow.setHeaderBodyHash();
			
			//Do so many then recalculate.. to have the latest block data
			long currentTime  = System.currentTimeMillis();
			
			//Keep cycling until it is ready 
			boolean mining = true;
			MiniData hash = null;
			while(mining && isRunning()) {
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow.getTxHeader());
				
				//Success ?
				if(hash.isLess(txpow.getBlockDifficulty())) {
					mining = false;
				}else {
					//Set the Nonce..
					txpow.setNonce(txpow.getNonce().increment());
					
					//Set the Time..
					txpow.setTimeMilli(new MiniNumber(currentTime));
				}
				
				//New time
				currentTime  = System.currentTimeMillis();
			}
			
			if(!isRunning()) {
				return;
			}

			//Set all the correct internal variables..
			txpow.calculateTXPOWID();
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("txpow", txpow);			
			
			//This MUST be the case..
			if(txpow.isBlock()) {
				InputHandler.endResponse(zMessage, true, "Block Mined");
				
				Message msg = new Message(ConsensusNet.CONSENSUS_NET_CHECKSIZE_TXPOW).addObject("txpow", txpow);
				getMainHandler().getConsensusHandler().PostMessage(msg);
			}else {
				InputHandler.endResponse(zMessage, false, "ERROR - debug miner failed to find a block..");
			}
			
		}else if(zMessage.isMessageType(TXMINER_TESTHASHING)) {
			//See how many hashes this machine can do..
			long timenow = System.currentTimeMillis();
			
			byte[] data = new byte[32];
			
			MinimaLogger.log("Testing hashing on this machine.. please wait..");
			
			for(int i=0;i<10000000;i++){
				//Now Hash it..
				byte[] hash = Crypto.getInstance().hashData(data);
			}
			
			double diff = (double)(System.currentTimeMillis()-timenow) / (double)1000;
			
			MinimaLogger.log("Finished. 10,000,000 hashes took "+diff+" seconds");
			
			//Speed
			double speed =   ( 10000000 / diff ) / 1000000;
			MinimaLogger.log("Speed : "+speed+" MHashes/sec");
		}	
	}
}
