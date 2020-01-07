package org.minima.system.maxima;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.messages.Message;

public class OffChainManager extends SystemHandler {

	public static final String OFFCHAIN_POW       = "OFFCHAIN_POW";
	public static final String OFFCHAIN_POW_VALID = "OFFCHAIN_POW_VALID";
	
	public static final String OFFCHAIN_MESSAGE   = "OFFCHAIN_MESSAGE";
	
	TxPOW mLatestPow = null;
	
	public OffChainManager(Main zMain) {
		super(zMain,"OffChainManager");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(OFFCHAIN_POW)) {
			//Get the TxPOW
			TxPOW txpow = (TxPOW)zMessage.getObject("txpow");
			
			//Check is Valid.. send through consensys..
//			PostMessage("CONSENSUS_CHECK_OFFCHAIN");
			PostMessage(OFFCHAIN_POW_VALID);
			
		}else if(zMessage.isMessageType(OFFCHAIN_POW_VALID)) {
			//Get the TxPOW
			TxPOW txpow = (TxPOW)zMessage.getObject("txpow");
			
			//Great! - check and store..
			mLatestPow = txpow;
			
		}else if(zMessage.isMessageType(OFFCHAIN_MESSAGE)) {
			//Check our latest TXPOW
			if(mLatestPow == null) {
				//ERRROR - not valid channel - Warn HIM!
				//for now..
				return;
			}
			
			//Check is Valid..
//			if(getMainHandler().getTopBlock().sub(mLatestPow.getBlockNumber()).isMore(new MiniNumber(256))) {
				//ERRROR - not valid channel - Warn HIM!
				//for now..
//				return;
//			}
			
			//Ok..it's a valid message.. padd it on..
			 
		}
		
	}

}
