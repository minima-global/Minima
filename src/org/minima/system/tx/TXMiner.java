package org.minima.system.tx;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.brains.ConsensusHandler;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class TXMiner extends SystemHandler{

	public static final MiniHash BASE_TXN 	= MiniHash.MAX_HASH;
	public static final MiniHash BASE_BLOCK = MiniHash.MAX_HASH;
	
	public static final String TXMINER_TESTHASHING = "MINE_TESTHASHING";
	public static final String TXMINER_MINETXPOW   = "MINE_MINETXPOW";
	public static final String TXMINER_MEGAMINER   = "MINE_MEGAMINER";
	
	boolean mAutoMining = false;
	
	public TXMiner(Main zMain) {
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
			TxPOW txpow = (TxPOW) zMessage.getObject("txpow");
			
			MiniNumber nonce = MiniNumber.ZERO;
			
			//And now start hashing.. 
			MiniHash hash = null;
			boolean mining 	= true;
			
			//Do so many then recalculate.. to have the latest block data
			long currentTime  = System.currentTimeMillis();
			
			//should be about 10..
			long maxTime  	  = currentTime + 10000;
			
			while(mining && currentTime < maxTime) {
				//Set the Nonce..
				txpow.setNonce(nonce);
				
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow);
				
				if(hash.isLess(txpow.getTxnDifficulty())) {
					//For Now..
					mining = false;
				}
				
				//Increment the nonce..
				nonce = nonce.increment();
				
				//New time
				currentTime  = System.currentTimeMillis();
			}
			
			//Did we find it.. ?
			if(mining) {
//				System.out.println("NOTFINISHED "+nonce);
				
				//Repost the same transaction.. get a new TxPOW block with latest details
				Message sametr = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
										.addObject("transaction", txpow.getTransaction())
										.addObject("witness", txpow.getWitness());

				//Send it..
				getMainHandler().getConsensusHandler().PostMessage(sametr);
				
			}else {
//				System.out.println("MINED! "+nonce);
				
				//Set the TxPOW
				txpow.calculateTXPOWID();
				
				//We have a valid TX-POW.. tell main
				Message msg = new Message(ConsensusHandler.CONSENSUS_PRE_PROCESSTXPOW).addObject("txpow", txpow);
				getMainHandler().getConsensusHandler().PostMessage(msg);
			}
			
//			System.out.println("FINISHED @ "+txdiff+" "+nonce);
			
		}else if(zMessage.isMessageType(TXMINER_MEGAMINER)) {
			//Get TXPOW..
			TxPOW txpow = (TxPOW) zMessage.getObject("txpow");
			
			//Do so many then recalculate.. to have the latest block data
			long currentTime  = System.currentTimeMillis();
			
			//should be about 10..
			long maxTime  	  = currentTime + 1000;
			
			//Keep cycling until it is ready 
			boolean mining = true;
			MiniHash hash = null;
			while(mining && currentTime<maxTime && isRunning()) {
				//Now Hash it..
				hash = Crypto.getInstance().hashObject(txpow);
				
				//Success ?
				if(hash.isLess(txpow.getBlockDifficulty())) {
					mining = false;
				}else {
					//Set the Nonce..
					txpow.setNonce(txpow.getNonce().increment());	
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
//				System.out.println("BLOCK FOUND : "+txpow.getBlockNumber());
				//We have a valid TX-POW.. tell main
				Message msg = new Message(ConsensusHandler.CONSENSUS_PRE_PROCESSTXPOW).addObject("txpow", txpow);
				getMainHandler().getConsensusHandler().PostMessage(msg);
			}
			
			//Pause for breath
			Thread.sleep(200);
			
			//And start the whole Mining thing again..
			getMainHandler().getConsensusHandler().PostMessage(ConsensusHandler.CONSENSUS_MINEBLOCK);
			
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
